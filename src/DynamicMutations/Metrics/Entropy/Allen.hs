module DynamicMutations.Metrics.Entropy.Allen where

import Control.Comonad      (extract, extend)

import DynamicMutations.Types.Tree (PhyloTree(PT))

branchLengths' :: Int ->  PhyloTree a -> PhyloTree (a, Int)
branchLengths' n (PT x ts) = PT (x, n) (branchLengths' (n+1) <$> ts)

branchEntropy :: Double -> (a -> Double) -> PhyloTree (a, Int) -> Double
branchEntropy tot fw t = let
        (_, l) = extract t
        p = sum (fw . fst <$> t) / tot
        in fromIntegral l * p * log p

entropy :: (a -> Double) -> PhyloTree a -> Double
entropy fw t = let
        tot = sum (fw <$> t)
        blt = branchLengths' 0 t
        in negate $ sum (extend (branchEntropy tot fw) blt)

