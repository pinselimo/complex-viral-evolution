{-# LANGUAGE BangPatterns #-}
module DynamicMutations.Metrics.Entropy.Jansson where

import Control.Arrow ((&&&))
import Control.Comonad (extend, extract)
import Data.Foldable (toList, foldMap', foldl')
import Data.List (sort)
import Data.Semigroup (Sum(getSum, Sum))

import DynamicMutations.Types.Tree (PhyloTree, children)

entropy :: PhyloTree a -> Double
entropy = entropyGeneral (length . children) (const 1)

entropyWeighted :: (a -> Double) -> PhyloTree a -> Double
entropyWeighted = entropyGeneral (length . children)

entropyGeneral :: (Eq b, Ord b) => (PhyloTree a -> b) -> (a -> Double) -> PhyloTree a -> Double
entropyGeneral fn fw t = let
        n = sum (fmap fw t)
        tl = foldl' groupSum [] . sort . toList $ extend (fn &&& Sum . fw . extract) t
        in getSum (foldMap' (\n_i -> Sum $ n_i / n * log (n / n_i)) (getSum . snd <$> tl))

groupSum :: (Eq a, Num b) => [(a, b)] -> (a, b) -> [(a, b)]
groupSum [] x = [x]
groupSum y@((yg, !yv):ys) x@(xg, xv) = if xg == yg then (yg, xv+yv):ys else x:y
