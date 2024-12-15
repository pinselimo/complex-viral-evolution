{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
module Repro where

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

import           DynamicMutations.Metrics.Linearity (getTotalImpact)
import           DynamicMutations.Types      (Result (..))
import           DynamicMutations.Types.PopulationGraph (unsafeVecSumX20)
import           DynamicMutations.Types.Tree (PhyloTree, broadcastWith, withTree)
import           Reproduction                (toResult, compressTree, simulateStrictW1)
import           Reproduction.Parameters     (Parameters (eqTime, mutability, decay), standardParameters)
import           Reproduction.Types          (Variant (recovered, mid))
import           Reproduction.Variant        (infected)
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
tEq = 100 * 365

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
     <> header "Reproduction model of Ferguson et al. 2003 published in Nature" )

parameters:: [Parameters]
parameters = [ standardParameters { mutability = m, eqTime = tEq, decay = d }
             | m <- [1e-5, 2.5e-6, 1e-6]
             , d <- [1/270, 2.0] -- Decay > 1 indicates no non-specific immunity
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
                Just n -> if n >= 0 && n < length parameters then [parameters !! n] else parameters

        forM_ (zip [fromMaybe 0 (run mopts)..] params) $ \(i, ps) -> do

                let put n (f :: a -> Double) tSim v =
                        let xs = [ show (mutability ps)
                                 , show (fromEnum n :: Int)
                                 , show tSim, show (f v)
                                 ]
                            in appendFileLn fname $ intercalate ";" xs

                let writer _ tSim' res = if tSim' < tEq
                                then return ()
                                else do
                                let tSim = tSim' - tEq
                                let !r = toResult ps tSim res
                                put (fromEnum Susceptible) totalSusceptible tSim r
                                put (fromEnum ShortLivedImmune)  totalShortLivedImmune tSim r
                                put (fromEnum Linearity) linearity tSim r
                                put (fromEnum JanssonEntropy) janssonEntropy tSim r
                                put (fromEnum JanssonEntropyWeighted) janssonWeighted tSim r
                                put (fromEnum Infected) totalInfected tSim r
                                put (fromEnum Recovered) totalRecovered tSim r
                                put (fromEnum Size) (fromIntegral . treeSize) tSim r
                                put (fromEnum Incidence) totalIncidence tSim r
                                put (fromEnum PairwiseDiversity) pairwiseDiversity tSim r
                                put (fromEnum Patch1Incidence) patch1Incidence tSim r
                                put (fromEnum BoostedImmunities) boostedImmunities tSim r
                                put (fromEnum UnweightedLinearity) unweightedLinearity tSim r
                                put (fromEnum MeanCrossImmunity) meanCrossImmunity tSim r
                                put (fromEnum MeanBeta) meanBeta tSim r
                                put (fromEnum MeanAlpha) meanAlpha tSim r
                                put (fromEnum MeanGamma) meanGamma tSim r
                                put (fromEnum AllenEntropy) allenEntropy tSim r
                                return ()

                sims <- simulateOneIO ps (U.singleton mseed) (simulateStrictW1 steps . writer)

                -- Store tree
                runIdentity sims & \(_,_,_,vt,it) -> do
                        let t = (vt V.!) <$> it
                        let count v = unsafeVecSumX20 (infected v + recovered v )
                        let top500 = sortBy (comparing Down) (toList $ count <$> t) !! min (length t-1) 500
                        let t' = compressTree (\tt -> getTotalImpact count tt >= top500) t
                        let byteTree = ET.toNewick $ toElynxTree (show . unsafeVecSumX20 . recovered) t'
                        BS.writeFile (fname <> "_mut." <> show (mutability ps)
                                            <> "repro"
                                            <.> "nwk") byteTree

                putStrLn $ "Finished simulation " ++ show i

toElynxTree :: (Variant -> String) -> PhyloTree Variant -> ET.Tree ET.Length String
toElynxTree doc t = let
        t'  = extend (((\tm -> if tm == 0 then tm else tm-tEq) . fst . fromMaybe (steps, 0) . mid &&& doc) . extract) t
        t'' = broadcastWith (0, doc (extract t)) (\(parentT, _) n -> (extract n, (fst (extract n) - parentT, snd (extract n)))) t'
        in withTree go t''
        where go (branch, name) ts = ET.Node (ET.toLengthUnsafe $ fromIntegral branch) name (withTree go <$> toList ts)

