{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE BangPatterns        #-}
module HPC where

import           Control.Arrow               ((&&&))
import           Control.Comonad             (Comonad(extend, extract))
import           Control.Monad               (forM_, when)
import           Control.Monad.Random.Strict (randomIO)
import qualified Data.ByteString.Lazy        as BS (writeFile)
import           Data.Foldable               (Foldable(toList))
import           Data.Function               ((&))
import           Data.Functor.Identity       (Identity (runIdentity))
import           Data.List                   (intercalate, sortBy)
import           Data.Maybe                  (fromMaybe)
import           Data.Ord                    (comparing, Down (Down))
import qualified Data.Vector                 as V ((!))
import qualified Data.Vector.Unboxed         as U (singleton)
import           Data.Word                   (Word32)
import           Options.Applicative         (Parser, strOption, showDefault, option, auto,
                                             long, short, metavar, help, value, (<**>), helper,
                                             info, fullDesc, header, ParserInfo, execParser)
import           System.Directory.Extra      (doesFileExist)
import           System.FilePath             (makeValid, (<.>), (</>))
import qualified ELynx.Tree                  as ET (Length,
                                                    Tree (Node),
                                                    toLengthUnsafe,
                                                    toNewick)

import           DynamicMutations            (toResult, compressTree, simulateStrictW1)
import           DynamicMutations.Metrics.Linearity (getTotalImpact)
import           DynamicMutations.Parameters (Parameters (distancingEffect, eqTime, mutability, threshold, cutoffDay, vaccRate),
                                              standardParameters)
import           DynamicMutations.Types      (Result (..), Variant(recovered, dead, mid))
import           DynamicMutations.Types.PopulationGraph (unsafeVecSumX20)
import           DynamicMutations.Types.Tree (PhyloTree, broadcastWith, withTree)
import           DynamicMutations.Variant    (infected)
import           Simulation                  (simulateOneIO)

data Options = Op
        { storagePath :: FilePath
        , seed :: Maybe Word32
        , run :: Maybe Int
        }

data StoredResult = Distancing
                  | Susceptible
                  | ShortLivedImmune
                  | Linearity
                  | JanssonEntropy
                  | JanssonEntropyWeighted
                  | Infected
                  | Recovered
                  | Dead
                  | Size
                  | Incidence
                  | PairwiseDiversity
                  | Patch1Incidence
                  | BoostedImmunities
                  | MeanCrossImmunity
                  | UnweightedLinearity
                  | MeanBeta
                  | MeanLambda
                  | MeanAlpha
                  | MeanGamma
                  | MeanMu
                  | MeanNu
                  | AllenEntropy
                  deriving (Show, Enum)

tEq :: Int
tEq = 0 * 365

steps :: Int
steps = tEq + 365*30

argParser :: Parser Options
argParser = Op
        <$> (makeValid <$> strOption (long "path" <> short 'p' <> metavar "PATH" <> help "Result storage path"))
        <*> option auto (long "seed" <> short 's' <> metavar "SEED" <> showDefault <> value Nothing <> help "Initial pseudo random number generator seed")
        <*> option auto (long "run" <> short 'r' <> metavar "INT" <> showDefault <> value Nothing <> help "Number of simulation run to perform")

opts :: ParserInfo Options
opts = info (argParser <**> helper)
      ( fullDesc
     <> header "Dynamic mutations with a SEPIARD model" )

parameters :: [Parameters]
parameters = concat $
           [ standardParameters { threshold = 999_999_999, distancingEffect = 1.0, mutability = m, eqTime = tEq, vaccRate = 0 }
               :
               [ standardParameters { threshold = t, distancingEffect = e, mutability = m, eqTime = tEq, cutoffDay = c, vaccRate = 0 }
               | c <- [2*365]
               , (e,t) <- [ (0.01, 1e-6), (0.1, 1e-6), (0.2, 1e-5)
                          , (0.05, 2.4e-5), (0.1, 2.5e-5), (0.25, 1e-5)
                          , (0.4, 5e-5), (0.4, 1e-6), (0.01, 5e-5)
                          ]
               ]
             | m <- [1e-5]
             ]

detailsParameters:: [Parameters]
detailsParameters = concat $
           [ standardParameters { threshold = 999_999_999, distancingEffect = 1.0, mutability = m, eqTime = tEq, vaccRate = 0 }
               :
               [ standardParameters { threshold = t, distancingEffect = e, mutability = m, eqTime = tEq, cutoffDay = c, vaccRate = 0 }
               | c <- [2*365]
               , e <- [0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4]
               , t <- [1e-6, 2.5e-6, 5e-6, 7.5e-6, 1e-5, 2.5e-5, 5e-5]
               ]
             | m <- [1e-5]
             ]

appendFileLn :: FilePath -> String -> IO ()
appendFileLn f s = appendFile f $ "\n" <> s

-- Get a seed, scan parameters, store all information and seed
main :: IO ()
main = do
        mopts <- execParser opts
        mseed <- maybe (randomIO :: IO Word32) return (seed mopts)
        let fname = storagePath mopts
                  </> "scan" <> show mseed
                  <.> "csv"
        {-
         -let shrink perc t = let
         -        count v = recovered v + dead v
         -        totalRec = sum ((\v -> recovered v + dead v) <$> t)
         -        in compressTree (\tt -> getTotalImpact count tt >= perc*totalRec) t
         -}

        -- Initialize files
        e <- doesFileExist fname
        when (not e) $ writeFile fname $ intercalate ";" ["Mutability", "CutoffDay", "Threshold", "DistancingEff", "Data", "Time", "Value"]

        -- Simulate range
        let params = case run mopts of
                Nothing -> parameters
                Just n -> if n >= 0 && n < length parameters then [parameters !! n]
                       else if n == -1 then detailsParameters else parameters

        forM_ (zip [fromMaybe 0 (run mopts)..] params) $ \(i, ps) -> do

                let put n (f :: a -> Double) tSim v =
                        let xs = [ show (mutability ps)
                                 , show (cutoffDay ps)
                                 , show (threshold ps)
                                 , show (distancingEffect ps)
                                 , show (fromEnum n :: Int)
                                 , show tSim, show (f v)
                                 ]
                            in appendFileLn fname $ intercalate ";" xs

                let writer _ tSim' res = if tSim' < tEq
                                then return ()
                                else do
                                let tSim = tSim' - tEq
                                let !r = toResult ps tSim res
                                put (fromEnum Distancing) (fromIntegral . distancingPatches) tSim r
                                put (fromEnum Susceptible) totalSusceptible tSim r
                                put (fromEnum ShortLivedImmune)  totalShortLivedImmune tSim r
                                put (fromEnum Linearity) linearity tSim r
                                put (fromEnum JanssonEntropy) janssonEntropy tSim r
                                put (fromEnum JanssonEntropyWeighted) janssonWeighted tSim r
                                put (fromEnum Infected) totalInfected tSim r
                                put (fromEnum Recovered) totalRecovered tSim r
                                put (fromEnum Dead) totalDead tSim r
                                put (fromEnum Size) (fromIntegral . treeSize) tSim r
                                put (fromEnum Incidence) totalIncidence tSim r
                                put (fromEnum PairwiseDiversity) pairwiseDiversity tSim r
                                put (fromEnum Patch1Incidence) patch1Incidence tSim r
                                put (fromEnum BoostedImmunities) boostedImmunities tSim r
                                put (fromEnum UnweightedLinearity) unweightedLinearity tSim r
                                put (fromEnum MeanCrossImmunity) meanCrossImmunity tSim r
                                put (fromEnum MeanBeta) meanBeta tSim r
                                put (fromEnum MeanLambda) meanLambda tSim r
                                put (fromEnum MeanAlpha) meanAlpha tSim r
                                put (fromEnum MeanGamma) meanGamma tSim r
                                put (fromEnum MeanMu) meanMu tSim r
                                put (fromEnum MeanNu) meanNu tSim r
                                put (fromEnum AllenEntropy) allenEntropy tSim r
                                return ()

                sims <- simulateOneIO ps (U.singleton mseed) (simulateStrictW1 steps . writer)

                -- Store tree
                runIdentity sims & \(_,_,_,_,vt,it,_) -> do
                        let t = (vt V.!) <$> it
                        let count v = unsafeVecSumX20 (infected v + recovered v + dead v)
                        let top500 = sortBy (comparing Down) (toList $ count <$> t) !! min (length t-1) 500
                        let t' = compressTree (\tt -> getTotalImpact count tt >= top500) t
                        let byteTree = ET.toNewick $ toElynxTree (\v -> show $ unsafeVecSumX20 (recovered v + dead v)) t'
                        BS.writeFile (fname <> "_mut." <> show (mutability ps)
                                            <> "_cut." <> show (cutoffDay ps)
                                            <> "_thr." <> show (threshold ps)
                                            <> "_eff." <> show (distancingEffect ps)
                                            <.> "nwk") byteTree

                putStrLn $ "Finished simulation " ++ show i

toElynxTree :: (Variant -> String) -> PhyloTree Variant -> ET.Tree ET.Length String
toElynxTree doc t = let
        t'  = extend (((\tm -> if tm == 0 then tm else tm-tEq) . fst . fromMaybe (steps, 0) . mid &&& doc) . extract) t
        t'' = broadcastWith (0, doc (extract t)) (\(parentT, _) n -> (extract n, (fst (extract n) - parentT, snd (extract n)))) t'
        in withTree go t''
        where go (branch, name) ts = ET.Node (ET.toLengthUnsafe $ fromIntegral branch) name (withTree go <$> toList ts)

