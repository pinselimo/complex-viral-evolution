{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns          #-}
module DynamicMutations where

import           Control.Arrow                       (Arrow (second))
import           Control.Comonad                     (extend, extract)
import           Control.Monad.Trans                 (MonadTrans)
import qualified Control.Monad.Trans.Random.Strict   as RS (RandT, evalRandT)
import           Control.Parallel.Strategies         (using, rpar)
import           Data.IntMap                         (fromAscList)
import           Data.Kind                           (Type)
import qualified Data.Map.Strict                     as M
import           Data.Maybe                          (fromJust)
import           Data.Primitive.SIMD                 (SIMDVector(indexVector, foldVector))
import           Data.Random                         (StatefulGen)
import qualified Data.Vector                         as V
import qualified Data.Vector.Unboxed                 as U

import           Simulation                              (simulateWriteTimedReps1)
import           DynamicMutations.Immunity               (randomStrain)
import           DynamicMutations.Metrics.Diversity      (nucleotideDiversity)
import qualified DynamicMutations.Metrics.Entropy.Allen   as Allen
import qualified DynamicMutations.Metrics.Entropy.Jansson as Jansson
import           DynamicMutations.Metrics.Linearity      (sizeCorrectedTreeImpactConcentration)
import           DynamicMutations.Parameters             (Parameters (eqTime, nAgents, nInit))
import           DynamicMutations.SIR                    (sirStep)
import           DynamicMutations.Types                  (ModelState, Result (..), Strain, initDeaths)
import           DynamicMutations.Types.PopulationGraph  (vecSum, vecMap, unsafeVecSumX20, GraphX20 (unVecX20), Vec (Vec), (&*))
import           DynamicMutations.Types.Tree             (PhyloTree (..),
                                                          toStrTree, treeDo, buildTree)
import           DynamicMutations.Variant                (Variant (..), baseVariant, evolveTree, infected, registerAll)

compressTree :: (PhyloTree a -> Bool) -> PhyloTree a -> PhyloTree a
compressTree p = fmap snd . compress fst . extend go
        where go t = (p t, extract t)

compress :: (a -> Bool) -> PhyloTree a -> PhyloTree a
compress p = treeDo go
        where go x ts = (x, V.filter (p . extract) $ treeDo go <$> ts)

modelStep :: forall (g :: Type) (m :: Type -> Type).
             (Monad m, StatefulGen g m) =>
             Parameters -> g -> Int -> ModelState -> m ModelState
modelStep ps gen time (d, s, sli, mdict, vt, t, ds) = if time < eqTime ps
             then let
            (_, s', sli', vt', !ds') = sirStep ps time d s sli mdict vt ds
                in return (Vec (U.generate 20 (const False)), s', sli', mdict, (\v -> v { mutationPotential = 0 }) <$> vt', t, ds')
             else do
             (vt', muts) <- evolveTree ps gen vt
             let (muts', mdict'@(_, _, !_strict)) = registerAll ps time (length vt) muts mdict
             let tMuts = fromAscList $ V.toList $ second (fmap (snd . fromJust . mid)) <$> muts'
             let (d', s', sli', vt'', !ds') = sirStep ps time d s sli mdict' (vt' V.++ (snd =<< muts')) ds
             return (d', s', sli', mdict', filtTree vt'', buildTree tMuts t, ds')

-- For performance reasons we assume that variants with less than 1e-65 infected hosts
-- will not re-emerge.
filtTree :: V.Vector Variant -> V.Vector Variant
filtTree = let
    go v = if unsafeVecSumX20 (infected v) >= 1e-65
         then v
         else v { exposed = 0, preSymptomatic = 0, asymptomatic = 0, symptomaticInfected = 0 }
    in V.map go

initState :: Parameters -> Strain -> ModelState
initState ps s = let
             t = V.fromList [(undefined, V.fromList [baseVariant ps s])]
             (t', mdict) = registerAll ps 0 0 t (M.empty, V.empty, V.empty)
             in (Vec (U.generate 20 (const False)), nAgents ps - nInit ps, 0, mdict, t' >>= snd, PT 0 V.empty, initDeaths)

simStepW1Strict :: (MonadTrans t, StatefulGen g (t IO)) => Int -> Parameters -> g -> (Int -> ModelState -> IO ()) -> t IO ModelState
simStepW1Strict ticks ps gen w = randomStrain ps gen >>= simulateWriteTimedReps1 w ticks (modelStep ps gen) . initState ps

simulateStrictW1 :: forall (g :: Type). (StatefulGen g (RS.RandT g IO)) =>
              Int -> (Int -> ModelState -> IO ()) -> Parameters -> g
              -> IO ModelState
simulateStrictW1 ticks w ps gen = RS.evalRandT (simStepW1Strict ticks ps gen w) gen

toResult :: Parameters -> Int -> ModelState -> Result
toResult _ps _time (d, s, sli, _, vt, it, _) = let
         wt = (wvt V.!) <$> it
         weightFunction v = unsafeVecSumX20 (infected v + recovered v)
         wvt = weightFunction <$> vt
         weighted f = fmap (\v -> infected v * f v) vt
         weighted' f = fmap (\v -> infected v &* f v) vt
         vt' = unsafeVecSumX20 . infected <$> vt
         thr = let
                m = sum vt' / fromIntegral (length vt')
                v = V.filter (<m) vt'
                in max 0.00001 $ min 1 $ sum v / fromIntegral (length v)
         isAnyGT1 = foldVector (\a b -> if a >= thr || b >= thr then 1 else 0)
         detectable = V.filter ((>=1) . isAnyGT1 . unVecX20 . infected) vt
        in Result
         { distancingPatches = vecSum (vecMap fromEnum d)
         , totalSusceptible = unsafeVecSumX20 s
         , totalShortLivedImmune = unsafeVecSumX20 (sum (shortLivedImmune <$> vt)) `using` rpar
         , linearity = sizeCorrectedTreeImpactConcentration id wt `using` rpar
         , janssonEntropy = Jansson.entropy it `using` rpar
         , janssonWeighted = Jansson.entropyWeighted id wt `using` rpar
         , totalInfected = sum (fmap (unsafeVecSumX20 . infected) vt) `using` rpar
         , totalRecovered = sum (fmap (unsafeVecSumX20 . recovered) vt) `using` rpar
         , totalDead =  sum (fmap (unsafeVecSumX20 . dead) vt) `using` rpar
         , treeSize = length vt
         , totalIncidence = sum (fmap (unsafeVecSumX20 . incidence) vt) `using` rpar
         , pairwiseDiversity = nucleotideDiversity detectable strain infected `using` rpar
         , patch1Incidence = sum (fmap (abs . (`indexVector` 0) . unVecX20 . incidence) vt) `using` rpar
         , boostedImmunities = unsafeVecSumX20 (sli - sum (shortLivedImmune <$> vt)) `using` rpar
         , unweightedLinearity = sizeCorrectedTreeImpactConcentration (const 1) wt `using` rpar
         , meanCrossImmunity = unsafeVecSumX20 (sum $ weighted immune) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanBeta = unsafeVecSumX20 (sum $ weighted' β) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanLambda = unsafeVecSumX20 (sum $ weighted' λ) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanAlpha = unsafeVecSumX20 (sum $ weighted' α) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanGamma = unsafeVecSumX20 (sum $ weighted' γ) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanMu = unsafeVecSumX20 (sum $ weighted' µ) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanNu = unsafeVecSumX20 (sum $ weighted' ν) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , allenEntropy = Allen.entropy id wt `using` rpar
         }

