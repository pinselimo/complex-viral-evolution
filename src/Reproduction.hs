{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns          #-}
module Reproduction where

import           Control.Arrow                       (Arrow (second))
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

import           Simulation                          (simulateWriteTimedReps1)
import           DynamicMutations.Immunity           (randomStrain)
import           DynamicMutations.Metrics.Diversity  (nucleotideDiversity)
import qualified DynamicMutations.Metrics.Entropy.Allen   as Allen
import qualified DynamicMutations.Metrics.Entropy.Jansson as Jansson
import           DynamicMutations.Metrics.Linearity  (sizeCorrectedTreeImpactConcentration)
import           DynamicMutations.Types              (Strain, Result(..))
import           DynamicMutations.Types.PopulationGraph  (unsafeVecSumX20, GraphX20 (unVecX20), (&*))
import           DynamicMutations.Types.Tree         (PhyloTree (..), buildTree)
import           Reproduction.Parameters             (Parameters (eqTime, nAgents, nInit))
import           Reproduction.SIR                    (sirStep)
import           Reproduction.Types                  (ModelState)
import           Reproduction.Variant                (Variant (..), baseVariant, evolveTree, infected, registerAll)

modelStep :: forall (g :: Type) (m :: Type -> Type).
             (Monad m, StatefulGen g m) =>
             Parameters -> g -> Int -> ModelState -> m ModelState
modelStep ps gen time (s, sli, mdict, vt, t) = if time < eqTime ps
             then let
            (s', sli', vt') = sirStep ps time s sli mdict vt
                in return (s', sli', mdict, (\v -> v { mutationPotential = 0 }) <$> vt', t)
             else do
             (vt', muts) <- evolveTree ps gen vt
             let (muts', mdict'@(_, _, !_strict)) = registerAll ps time (length vt) muts mdict
             let tMuts = fromAscList $ V.toList $ second (fmap (snd . fromJust . mid)) <$> muts'
             let (s', sli', vt'') = sirStep ps time s sli mdict' (vt' V.++ (snd =<< muts'))
             return (s', sli', mdict', filtTree vt'', buildTree tMuts t)

filtTree :: V.Vector Variant -> V.Vector Variant
filtTree = let
    go v = if unsafeVecSumX20 (infected v) >= 1e-70
         then v
         else v { exposed = 0, symptomaticInfected = 0 }
    in V.map go

initState :: Parameters -> Strain -> ModelState
initState ps s = let
             t = V.fromList [(undefined, V.fromList [baseVariant ps s])]
             (t', mdict) = registerAll ps 0 0 t (M.empty, V.empty, V.empty)
             in (nAgents ps - nInit ps, 0, mdict, t' >>= snd, PT 0 V.empty)

simStepW1Strict :: (MonadTrans t, StatefulGen g (t IO)) => Int -> Parameters -> g -> (Int -> ModelState -> IO ()) -> t IO ModelState
simStepW1Strict ticks ps gen w = randomStrain ps gen >>= simulateWriteTimedReps1 w ticks (modelStep ps gen) . initState ps

simulateStrictW1 :: forall (g :: Type). (StatefulGen g (RS.RandT g IO)) =>
              Int -> (Int -> ModelState -> IO ()) -> Parameters -> g
              -> IO ModelState
simulateStrictW1 ticks w ps gen = RS.evalRandT (simStepW1Strict ticks ps gen w) gen

toResult :: Parameters -> Int -> ModelState -> Result
toResult _ps _time (s, sli, _, vt, it) = let
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
         { distancingPatches = 0
         , totalSusceptible = unsafeVecSumX20 s
         , totalShortLivedImmune = unsafeVecSumX20 (sum (shortLivedImmune <$> vt)) `using` rpar
         , linearity = sizeCorrectedTreeImpactConcentration id wt `using` rpar
         , janssonEntropy = Jansson.entropy it `using` rpar
         , janssonWeighted = Jansson.entropyWeighted id wt `using` rpar
         , totalInfected = sum (fmap (unsafeVecSumX20 . infected) vt) `using` rpar
         , totalRecovered = sum (fmap (unsafeVecSumX20 . recovered) vt) `using` rpar
         , totalDead = 0
         , treeSize = length vt
         , totalIncidence = sum (fmap (unsafeVecSumX20 . incidence) vt) `using` rpar
         , pairwiseDiversity = nucleotideDiversity detectable strain infected `using` rpar
         , patch1Incidence = sum (fmap (abs . (`indexVector` 0) . unVecX20 . incidence) vt) `using` rpar
         , boostedImmunities = unsafeVecSumX20 (sli - sum (shortLivedImmune <$> vt)) `using` rpar
         , unweightedLinearity = sizeCorrectedTreeImpactConcentration (const 1) wt `using` rpar
         , meanCrossImmunity = unsafeVecSumX20 (sum $ weighted immune) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanBeta = unsafeVecSumX20 (sum $ weighted' β) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanLambda = -1
         , meanAlpha = unsafeVecSumX20 (sum $ weighted' α) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanGamma = unsafeVecSumX20 (sum $ weighted' γ) / unsafeVecSumX20 (sum (infected <$> vt)) `using` rpar
         , meanMu = -1
         , meanNu = -1
         , allenEntropy = Allen.entropy id wt `using` rpar
         }

