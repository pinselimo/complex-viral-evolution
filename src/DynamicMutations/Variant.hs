{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE BangPatterns          #-}
module DynamicMutations.Variant
       (Variant (..), varStep, evolveTree, infected, baseVariant, calcImmunities, calcImmunitiesV, calcImmunities', getImmune, strainFlows, strainCrossImmunity, registerAll, fID, calcInfectedV, calcBoostedV, varStepNoInf)
       where

import           Prelude                           hiding (exp, init)

import           Control.Arrow                     (second, (&&&))
import           Control.Monad                     (replicateM, forM)
import           Control.Monad.Random              (mkStdGen)
import           Control.Monad.State.Strict        (State, runState)
import           Control.Parallel.Strategies       (using, rdeepseq, parTraversable)
import           Data.Foldable                     (fold, foldl')
import           Data.Kind                         (Type)
import           Data.Maybe                        (fromJust)
import           Data.Primitive.SIMD               (SIMDVector(zipVector))
import           Data.Random                       (StatefulGen, normal,
                                                    sampleFrom, shuffleNofM)
import           Data.Random.Distribution.Binomial (binomial)
import           Data.Random.Vector                (randomElement)
import           Data.Semigroup                    (Sum (..))
import qualified Data.Sequence                     as Seq
import           Data.Tuple.Extra                  (thd3, fst3)
import qualified Data.Vector                       as V
import qualified Data.Vector.Unboxed               as U
import           System.Random.MWC                 (Uniform(uniformM))
import           System.Random.Stateful            (runSTGen_)

import           Simulation                  (chunksOf)
import           DynamicMutations.Immunity   (bases, unsafeFastImmunityUp, isStop,
                                              register, fastImmunity, effStrainToHshAmino)
import           DynamicMutations.ODEs
import           DynamicMutations.Parameters (Parameters (initµ, initα, initβ, initγ, initλ, initν, mutability, nCodons, nEpitopes, nInit, σI, decay, nAgents))
import           DynamicMutations.Types
import           DynamicMutations.Types.PopulationGraph (convert, GraphX20 (unVecX20, VecX20), oneX20, unpackX20, vecMapX20, (&*), nPatches, unsafeVecSumX20)
import           DynamicMutations.Types.Tree (PhyloTree (..))

getImmune :: PhyloTree Variant -> GraphX20
getImmune = (getSum . fold) . fmap (Sum . recovered)

boostFlows :: Variant -> (GraphX20, Int)
boostFlows v = (boostedCrs v, fromJust (astrain v))

strainCrossBoosted :: CICache -> U.Vector (GraphX20, Int) -> Int -> GraphX20
strainCrossBoosted imm vs s = let
         imf = unsafeFastImmunityUp imm
        in U.foldl' (+) 0 (U.map (go (\s' -> if s >= s' then imf s s' else imf s' s)) vs)
        where
          go immfunc (x', s') = x' &* immfunc s'
          {-# INLINE go #-}

calcBoostedV :: ImmunityDict -> V.Vector Variant -> Int -> GraphX20
calcBoostedV (_,_,imm) t = let
        t' = boostFlows <$> t
        !t'' = U.convert (V.filter ((>=1e-5) . unsafeVecSumX20 . fst) t')
        in strainCrossBoosted imm t''

infFlows :: Variant -> (GraphX20, Int)
infFlows v = (infected v + shortLivedImmune v, fromJust (astrain v))

strainCrossInfected :: CICache -> U.Vector (GraphX20, Int) -> Int -> GraphX20
strainCrossInfected imm vs s = let
         imf = unsafeFastImmunityUp imm
         (vsLT, vsR) = U.span ((<=s) . snd) vs
         (vsGT, vs') = U.span ((>=s) . snd) vsR
        in U.foldl' (+) 0 (U.map (go (\s' -> if s >= s' then imf s s' else imf s' s)) vs')
         + U.foldl' (+) 0 (U.map (go (imf s))   vsLT)
         + U.foldl' (+) 0 (U.map (go (`imf` s)) vsGT)
        where
          go immfunc (x', s') = x' &* (1 - immfunc s')
          {-# INLINE go #-}

calcInfectedV :: ImmunityDict -> V.Vector Variant -> Int -> GraphX20
calcInfectedV (_,_,imm) t = let
        t' = infFlows <$> t
        !t'' = U.convert (V.filter ((>=1e-5) . unsafeVecSumX20 . fst) t')
        in strainCrossInfected imm t''

strainFlows' :: Variant -> (Bool, GraphX20, GraphX20, Int)
strainFlows' v = (initialized v, recovered v + immune' v, immune' v, fromJust (astrain v))

strainFlows :: Variant -> (GraphX20, GraphX20, Int)
strainFlows v = (recovered v + immune' v, immune' v, fromJust (astrain v))

strainCrossImmunity :: CICache -> U.Vector (GraphX20, GraphX20, Int) -> Bool -> Int -> GraphX20
strainCrossImmunity imm vs init s = let
         imf = unsafeFastImmunityUp imm
         (vsLT, vsR) = U.span ((<=s) . thd3) vs
         (vsGT, vs') = U.span ((>=s) . thd3) vsR
        in U.foldl' (+) 0 (U.map (go (\s' -> if s >= s' then imf s s' else imf s' s)) vs')
         + U.foldl' (+) 0 (U.map (go (imf s))   vsLT)
         + U.foldl' (+) 0 (U.map (go (`imf` s)) vsGT)
        where
          go immfunc (x', y', s') = (if init then y' else x') &* immfunc s'
          {-# INLINE go #-}

strainCrossImmunitySafe :: CICache -> (Bool, GraphX20, GraphX20, Int) -> V.Vector (Bool, GraphX20, GraphX20, Int) -> GraphX20
strainCrossImmunitySafe imm (init, _, _, s) = U.foldl' (+) 0 . U.convert . V.map go
        where
          go (_, x', y', s') = (if init then y' else x') &* fastImmunity imm s s'
          {-# INLINE go #-}

calcImmunitiesV :: ImmunityDict -> V.Vector Variant -> Bool -> Int -> GraphX20
calcImmunitiesV (_,_,imm) t = let
        t' = strainFlows <$> t
        !t'' = U.convert (V.filter ((>=1e-5) . unsafeVecSumX20 . fst3) t')
        in strainCrossImmunity imm t''

calcImmunities :: ImmunityDict -> PhyloTree Variant -> PhyloTree (Variant, GraphX20)
calcImmunities (_,_,imm) = calcImmunities' strainFlows' (strainCrossImmunitySafe imm)

calcImmunities' :: (Variant -> a) -> (a -> V.Vector a -> GraphX20) -> PhyloTree Variant -> PhyloTree (Variant, GraphX20)
calcImmunities' convFunc immFunc t = let
        t' = fmap (id &&& convFunc) t
        in fmap (second (`immFunc` V.fromList (foldl' (flip ((:) . snd)) [] t'))) t'

infected :: Variant -> GraphX20
infected v = sum (map ($ v) [exposed, preSymptomatic, asymptomatic, symptomaticInfected])

baseVariant :: Parameters -> Strain -> Variant
baseVariant ps s = Variant (initα ps) (initβ ps) (initγ ps) (initλ ps) (initµ ps) (initν ps)
                       0 0 (nInit ps) 0 0 0 0 0 0 0 0 0 0 0 0 Nothing (strHsh s) Nothing False


registerAll :: Parameters -> Int -> Int -> V.Vector (Int, V.Vector Variant) -> ImmunityDict -> (V.Vector (Int, V.Vector Variant), ImmunityDict)
registerAll ps time offset muts d = let
        in second snd $ runState (act muts) (offset, d)
        where act :: V.Vector (Int, V.Vector Variant) -> State (Int, ImmunityDict) (V.Vector (Int, V.Vector Variant))
              act = traverse (\(parent, ms) -> do
                                        ids <- traverse (register ps . effStrainToHshAmino . strain) ms
                                        return (parent, V.zipWith (\v (ix, i) -> v { mid = Just (time, ix), astrain = Just i }) ms ids)
                                        )

fID :: Variant -> Int
fID = maybe (error "Unidentified variant") snd . mid
{-# INLINE fID #-}

cap :: Parameters -> GraphX20 -> GraphX20
cap ps = vecMapX20 (max 0.0) . VecX20 . zipVector min (unVecX20 (nAgents ps)) . unVecX20
{-# INLINE cap #-}

varStepNoInf :: Parameters -> Variant -> Variant
varStepNoInf ps v = let
        noSLI = decay ps > 1.0
        l = shortLivedImmune v
        b = boostedCrs v
        sli = integrate (dimm ps l) l
        boo = integrate (dimm ps b) b
        in v { immune' = if noSLI then 0 else l - sli
             , shortLivedImmune = if noSLI then 0 else sli
             , boostedCrs = if noSLI then 0 else boo
             }

varStep :: Parameters -> Int -> Susceptible -> Immune -> Immune -> GraphX20 -> GraphX20 -> Distancing -> Variant -> GraphX20 -> Variant
varStep ps time s totalSli boosted mInf fRec dist v mimmune' = let
        e = exposed v
        p = preSymptomatic v
        a = asymptomatic v
        i = symptomaticInfected v
        r = recovered v
        c = natDeathRate ps $ integrate mimmune' (immune v)
        d = dead v
        l = shortLivedImmune v
        b = boostedCrs v
        noSLI = decay ps > 1.0

        sReal = cap ps $ s - c + boosted + mInf * c / nAgents ps
        s' = ds ps time v dist sReal i p a
        immuneBoostedSusceptible = ds ps time v dist (cap ps $ s - sReal) i p a
        immuneBoostedNonspecifics = ds ps time v dist totalSli i p a

        exp = integrate (de ps time v dist sReal i p a e) e
        pre = integrate (dp v e p) p
        asy = integrate (da v p a) a
        inf = integrate (di v p i) i
        sli = integrate (dimm ps l) l
        boo = integrate (dimm ps (b-(-immuneBoostedNonspecifics)*b/totalSli)) b
        rec = integrate (dr ps v a i fRec) r
        imm = c
        ded = integrate (dd ps v i fRec) d

        in v { susceptible' = if noSLI then s' else s' + immuneBoostedSusceptible
             , immune' = if noSLI then rec-r else l - sli
             , exposed = exp
             , preSymptomatic = pre
             , asymptomatic = asy
             , symptomaticInfected = inf
             , recovered = rec
             , immune = imm
             , shortLivedImmune = if noSLI then 0 else sli + (rec - r)
             , shortLivedImmuneSus' = if noSLI then 0 else (-immuneBoostedSusceptible) + (rec - r) -- TODO: Handle immune Boosted nonspecifics
             , shortLivedImmuneNon' = if noSLI then 0 else -immuneBoostedNonspecifics
             , boostedCrs = if noSLI then 0 else boo+(-immuneBoostedSusceptible)
             , dead = ded
             , incidence = -s'
             , initialized = True
             }


mutateBase :: StatefulGen g m => g -> Strain -> [(Int, Sel)] -> m Strain
mutateBase _   s [] = return $! s
mutateBase gen s ((codon, base):idcs) = do
        let (_a, _b, _c) = s `Seq.index` codon
        let (b, f) = case base of
                        X -> (_a, (,_b,_c))
                        Y -> (_b, (_a,,_c))
                        Z -> (_c, (_a,_b,))
        let possibleCodons = V.filter (not . isStop) (f <$> V.filter (/= b) bases)
        !c' <- sampleFrom gen (randomElement possibleCodons)
        mutateBase gen (Seq.update codon c' s) idcs

mutateBases :: StatefulGen g m => Parameters -> g -> Int -> Strain -> m Strain
mutateBases ps gen n s = do
        idcs <- sampleFrom gen (shuffleNofM n (nCodons ps * nEpitopes ps * 3) [(x,y) | x <- [0..nCodons ps * nEpitopes ps - 1], y <- [X, Y, Z]])
        mutateBase gen s idcs

mutateStrain :: StatefulGen g m => Parameters -> g -> Strain -> Int -> m (V.Vector Strain)
mutateStrain _  _   _ 0 = return V.empty
mutateStrain ps gen s 1 = do
        s'   <- mutateBases ps gen 1 s
        return $! V.singleton s'
mutateStrain ps gen s n = do
        nAdd <- sampleFrom gen (binomial (nCodons ps * nEpitopes ps * 3 - 1) (mutability ps))
        s'   <- mutateBases ps gen (min n (nAdd+1)) s
        ss   <- mutateStrain ps gen s (max 0 (n-1-nAdd))
        return $! s' `V.cons` ss

mutate :: forall (g :: Type) (m :: Type -> Type). StatefulGen g m =>
          Parameters -> g -> Variant -> Int -> Strain -> m Variant
mutate ps gen v i s' = let
    bounded = max 0.0 . min 1.0
    m x = do
        n <- sampleFrom gen (normal 0 (σI ps))
        return $! x * if n > -1 then 1+n else 0.01
    in do
     α' <- m (α v)
     β' <- m (β v)
     γ' <- m (γ v)
     λ' <- m (λ v)
     µ' <- m (µ v)
     ν' <- m (ν v)
     return $! Variant α' β' γ' (bounded λ') µ' (bounded ν')
                  0 0 0 (oneX20 i &* 1) 0 0 0 0 0 0 0 0 0 0 0 Nothing (strHsh s') Nothing False

evolveVariant :: forall (g :: Type) (m :: Type -> Type).
          StatefulGen g m => Parameters -> g -> Variant -> m (Variant, V.Vector Variant)
evolveVariant ps gen v =
       let
       nSampless = V.zip (V.fromListN nPatches [0..]) $ convert $ unpackX20
                 $ mutationPotential v * fromIntegral (nCodons ps * nEpitopes ps * 3)

       in if unsafeVecSumX20 (mutationPotential v) == 0
       then return (v, V.empty)
       else do
        muts <- forM nSampless $ \(patch, nSamples) -> do
          nMutate <- round <$> sampleFrom gen (binomial (max 0.0 nSamples) (mutability ps))
          strainMuts <- mutateStrain ps gen (strUnHsh $ strain v) nMutate
          muts <- traverse (mutate ps gen v patch) strainMuts
          return $! muts

        let
           muts' = V.foldl' (<>) mempty muts
          in return (v { preSymptomatic = preSymptomatic v - sum (preSymptomatic <$> muts')
                  , asymptomatic = asymptomatic v - sum (asymptomatic <$> muts')
                  , symptomaticInfected = symptomaticInfected v - sum (symptomaticInfected <$> muts')
                  , mutationPotential = 0
                  }
               , muts'
               )

evolveTree :: forall (g :: Type) (m :: Type -> Type).
          StatefulGen g m => Parameters -> g -> V.Vector Variant -> m (V.Vector Variant, V.Vector (Int, V.Vector Variant))
evolveTree ps gen vt = let
        chunkSize = max 2 $ V.length vt `quot` 20
        nThreads  = let (n,r) = V.length vt `quotRem` chunkSize in n + if r > 0 then 1 else 0
        in do
             gens <- fmap mkStdGen <$> replicateM nThreads (uniformM gen)
             let
                vs' :: V.Vector (Variant, V.Vector Variant)
                vs' = let go (g, vs) = runSTGen_ g (\g' -> traverse (evolveVariant ps g') vs)
                       in V.concat ((go <$> zip gens (chunksOf chunkSize vt))
                                    `using` parTraversable rdeepseq)

             return (fst <$> vs', (snd . fromJust . mid . fst &&& snd) <$> vs')

