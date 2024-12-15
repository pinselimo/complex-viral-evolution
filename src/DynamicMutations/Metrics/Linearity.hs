module DynamicMutations.Metrics.Linearity where

import           Control.Comonad      (extend)
import           Data.Foldable        (foldl')
import           Data.Foldable.Extra  (sum')

import           DynamicMutations.Types.Tree (PhyloTree (..), withTree)

getTotalImpact :: (a -> Double) -> PhyloTree a -> Double
getTotalImpact fw = withTree go
  where go v ts = fw v + sum' (getTotalImpact fw <$> ts)

getMaxImpact :: (a -> Double) -> PhyloTree a -> Double
getMaxImpact fw = withTree go
  where go _ ts = foldl' max 0 (getTotalImpact fw <$> ts)

sizeCorrectedTreeImpactConcentration :: (a -> Double) -> PhyloTree a -> Double
sizeCorrectedTreeImpactConcentration fw t = (sum(fw <$> t) + sum(extend (getMaxImpact fw) t)) / sum(extend (getTotalImpact fw) t)

