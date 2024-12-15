{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
module DynamicMutations.ODEs where

import           GHC.Exts             (DoubleX4#, broadcastDoubleX4#, Double(D#))

import           DynamicMutations.Parameters (Parameters (decay, distancingEffect, nAgents, ψII, epsilon, homophily, lifespan))
import           DynamicMutations.Types      (Variant (..), Distancing)
import           DynamicMutations.Types.PopulationGraph

euler :: DoubleX4# -> GraphX20 -> GraphX20 -> GraphX20
euler h dx x = x + dx &*# h
{-# INLINE euler #-}
stdEuler :: GraphX20 -> GraphX20 -> GraphX20
stdEuler = let !(D# d) = 1 in euler (broadcastDoubleX4# d)
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

socialDistancing :: Parameters -> Distancing -> GraphX20
socialDistancing ps = packX20 . vecMap (\d -> if d then distancingEffect ps else 1.0)

sumOthers :: GraphX20 -> GraphX20
sumOthers is = unsafeVecSumX20 is -& is

seasonality :: Parameters -> Int -> GraphX20
seasonality ps t = 1 + epsilon ps &* sin (fromIntegral t * (2 * pi / 365))

pVec :: Parameters -> GraphX20
pVec ps = let g = nAgents ps in g &/ unsafeVecSumX20 g

ds :: Parameters -> Int -> Variant -> Distancing -> GraphX20 -> GraphX20 -> GraphX20 -> GraphX20 -> GraphX20
ds ps time v dist s i p a = let infectious = seasonality ps time  * socialDistancing ps dist* (i+p+a) / nAgents ps
                                h = homophily ps :: GraphX20
                                pVec' = (1-h) * pVec ps
                         in -s &* β v * ((h + pVec') * infectious + sumOthers (pVec' * infectious))
dimm :: Parameters -> GraphX20 -> GraphX20
dimm ps x = (- decay ps) * x
de :: Parameters -> Int -> Variant -> Distancing -> GraphX20 -> GraphX20 -> GraphX20 -> GraphX20 -> GraphX20 -> GraphX20
de ps time v dist s i p a e = (- ds ps time v dist s i p a) - e &* α v
dp :: Variant -> GraphX20 -> GraphX20 -> GraphX20
dp v e p = e &* α v - p &* µ v
da :: Variant -> GraphX20 -> GraphX20 -> GraphX20
da v p a = p &* (µ v * (1- ν v)) - a &* γ v
di :: Variant -> GraphX20 -> GraphX20 -> GraphX20
di v p i = p &* (µ v * ν v) - i &* γ v
dr :: Parameters -> Variant -> GraphX20 -> GraphX20 -> GraphX20 -> GraphX20
dr ps v a i fcp = (a + i *(λ v +& fcp &* (1-λ v)*ψII ps)) &* γ v
{-# INLINE dr #-}
dd :: Parameters -> Variant -> GraphX20 -> GraphX20 -> GraphX20
dd ps v i fcp =  i * (1-(λ v +& fcp &* (1-λ v)*ψII ps)) &* γ v

