{-# LANGUAGE BangPatterns #-}
module DynamicMutations.Metrics.Diversity where

import           Control.Arrow ((&&&))
import           Data.Foldable (Foldable(toList, foldMap'))
import           Data.Semigroup (Sum(getSum, Sum))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import DynamicMutations.Types (EffStrain, compCodonW8)
import DynamicMutations.Types.PopulationGraph (GraphX20, unsafeVecSumX20)

nucleotideDiversity :: V.Vector a -> (a -> EffStrain) -> (a -> GraphX20) -> Double
nucleotideDiversity t strain fw = let
       tl  = (strain &&& unsafeVecSumX20 . fw) <$> toList t
       in let d = getSum (pairWs tl) in if d == 0 then 0 else getSum (pairDiffs tl) / d
       where
        pairDiffs [] = Sum 0
        pairDiffs ((i,w1):xs) = let nd (j,w2) = w1 * w2 * fromIntegral (U.sum $ U.zipWith compCodonW8 i j) in foldMap' (Sum . nd) xs <> pairDiffs xs
        pairWs [] = Sum 0
        pairWs ((_,!w):xs) = Sum (w*w) <> foldMap' (Sum . (w*) . snd) xs <> pairWs xs

