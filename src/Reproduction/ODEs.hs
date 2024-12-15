{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
module Reproduction.ODEs where

import           GHC.Exts             (DoubleX4#, broadcastDoubleX4#, Double(D#))

import           DynamicMutations.Types.PopulationGraph
import           Reproduction.Parameters (Parameters (decay, nAgents, epsilon, homophily, lifespan))
import           Reproduction.Types      (Variant (..))

euler :: DoubleX4# -> GraphX20 -> GraphX20 -> GraphX20
euler h dx x = x + dx &*# h
{-# INLINE euler #-}
paramEuler :: GraphX20 -> GraphX20 -> GraphX20
paramEuler = let !(D# d) = 1 in euler (broadcastDoubleX4# d)
{-# INLINE paramEuler #-}
stdEuler :: GraphX20 -> GraphX20 -> GraphX20
stdEuler = (+)
{-# INLINE stdEuler #-}
integrate :: GraphX20 -> GraphX20 -> GraphX20
integrate a = vecMapX20 (max 0.0) . stdEuler a
{-# INLINE integrate #-}

natBirthRate :: Parameters -> GraphX20 -> GraphX20
natBirthRate ps = integrate (nAgents ps &/ fromIntegral (lifespan ps))
{-# INLINE natBirthRate #-}

natDeathRate :: Parameters -> GraphX20 -> GraphX20
natDeathRate ps x = integrate (- (x &/ fromIntegral (lifespan ps))) x
{-# INLINE natDeathRate #-}

sumOthers :: GraphX20 -> GraphX20
sumOthers is = unsafeVecSumX20 is -& is

seasonality :: Parameters -> Int -> GraphX20
seasonality ps t = 1 + epsilon ps &* sin (fromIntegral t * (2 * pi / 365))

pVec :: Parameters -> GraphX20
pVec ps = let g = nAgents ps in g &/ unsafeVecSumX20 g

ds :: Parameters -> Int -> Variant -> GraphX20 -> GraphX20 -> GraphX20
ds ps time v s i = let infectious = seasonality ps time  * i / nAgents ps
                       h = homophily ps :: GraphX20
                       pVec' = (1-h) * pVec ps
                         in -s &* β v * ((h + pVec') * infectious + sumOthers (pVec' * infectious))
dimm :: Parameters -> GraphX20 -> GraphX20
dimm ps x = (- decay ps) * x
de :: Parameters -> Int -> Variant -> GraphX20 -> GraphX20 -> GraphX20 -> GraphX20
de ps time v s i e = (- ds ps time v s i) - e &* α v
di :: Variant -> GraphX20 -> GraphX20 -> GraphX20
di v e i = e &* α v - i &* γ v
dr :: Variant -> GraphX20 -> GraphX20
dr v i = i &* γ v
{-# INLINE dr #-}

