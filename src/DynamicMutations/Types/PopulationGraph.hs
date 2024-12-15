{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MagicHash                  #-}
module DynamicMutations.Types.PopulationGraph where

import           Control.DeepSeq      (NFData(rnf))
import           Data.Monoid          (Sum(Sum))
import           Data.Primitive.SIMD  (DoubleX20(..), SIMDVector (broadcastVector, mapVector, generateVector, unsafeIndexVector, sumVector))
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U
import           GHC.Exts             (timesDoubleX4#, DoubleX4#, Double(D#), broadcastDoubleX4#, plusDoubleX4#, minusDoubleX4#, divideDoubleX4#)
import           GHC.Generics         (Generic)
import qualified Data.Vector.Generic  as VG (Vector)
import qualified Data.Vector.Generic.Mutable as VGM (MVector)

nPatches :: Int
nPatches = 20

newtype Vec a = Vec {
        unVec :: U.Vector a
        } deriving (Show, Eq, Ord, Generic, NFData)

type Graph = Vec Double

newtype GraphX20 = VecX20 {
        unVecX20 :: DoubleX20
        } deriving (Show, Eq, Ord, Generic, Num, Floating, Fractional)

instance NFData GraphX20 where
        rnf (VecX20 (DoubleX20 !_ !_ !_ !_ !_)) = ()

newtype instance U.MVector s GraphX20 = MV_GraphX20 (U.MVector s DoubleX20)
newtype instance U.Vector    GraphX20 = V_GraphX20  (U.Vector    DoubleX20)
deriving instance VGM.MVector U.MVector GraphX20
deriving instance VG.Vector   U.Vector  GraphX20
instance U.Unbox GraphX20

multGraphD :: GraphX20 -> Double -> GraphX20
multGraphD v (D# d#) = let !d = broadcastDoubleX4# d# in multGraph v d
{-# INLINE multGraphD #-}

multGraph :: GraphX20 -> DoubleX4# -> GraphX20
multGraph (VecX20 (DoubleX20 m1 m2 m3 m4 m5)) d =
           VecX20 (DoubleX20 (timesDoubleX4# m1 d) (timesDoubleX4# m2 d) (timesDoubleX4# m3 d) (timesDoubleX4# m4 d) (timesDoubleX4# m5 d))
{-# INLINE multGraph #-}

divGraphD :: GraphX20 -> Double -> GraphX20
divGraphD v (D# d#) = let !d = broadcastDoubleX4# d# in divGraph v d
{-# INLINE divGraphD #-}

divGraph :: GraphX20 -> DoubleX4# -> GraphX20
divGraph (VecX20 (DoubleX20 m1 m2 m3 m4 m5)) d =
           VecX20 (DoubleX20 (divideDoubleX4# m1 d) (divideDoubleX4# m2 d) (divideDoubleX4# m3 d) (divideDoubleX4# m4 d) (divideDoubleX4# m5 d))
{-# INLINE divGraph #-}

addGraphD :: Double -> GraphX20 -> GraphX20
addGraphD (D# d#) v = let !d = broadcastDoubleX4# d# in addGraph d v
{-# INLINE addGraphD #-}

addGraph :: DoubleX4# -> GraphX20 -> GraphX20
addGraph d (VecX20 (DoubleX20 m1 m2 m3 m4 m5)) =
           VecX20 (DoubleX20 (plusDoubleX4# d m1) (plusDoubleX4# d m2) (plusDoubleX4# d m3) (plusDoubleX4# d m4) (plusDoubleX4# d m5))
{-# INLINE addGraph #-}

subGraphD :: Double -> GraphX20 -> GraphX20
subGraphD (D# d#) v = let !d = broadcastDoubleX4# d# in subGraph d v
{-# INLINE subGraphD #-}

subGraph :: DoubleX4# -> GraphX20 -> GraphX20
subGraph d (VecX20 (DoubleX20 m1 m2 m3 m4 m5)) =
           VecX20 (DoubleX20 (minusDoubleX4# d m1) (minusDoubleX4# d m2) (minusDoubleX4# d m3) (minusDoubleX4# d m4) (minusDoubleX4# d m5))
{-# INLINE subGraph #-}

(-&) :: Double -> GraphX20 -> GraphX20
(-&) = subGraphD
infixl 6 -&
{-# INLINE (-&) #-}

(-&#) :: DoubleX4# -> GraphX20 -> GraphX20
(-&#) = subGraph
infixl 6 -&#
{-# INLINE (-&#) #-}

(+&) :: Double -> GraphX20 -> GraphX20
(+&) = addGraphD
infixl 6 +&
{-# INLINE (+&) #-}

(+&#) :: DoubleX4# -> GraphX20 -> GraphX20
(+&#) = addGraph
infixl 6 +&#
{-# INLINE (+&#) #-}

(&/) :: GraphX20 -> Double -> GraphX20
(&/) = divGraphD
infixl 7 &/
{-# INLINE (&/) #-}

(&/#) :: GraphX20 -> DoubleX4# -> GraphX20
(&/#) = divGraph
infixl 7 &/#
{-# INLINE (&/#) #-}

(&*) :: GraphX20 -> Double -> GraphX20
(&*) = multGraphD
infixl 7 &*
{-# INLINE (&*) #-}

(&*#) :: GraphX20 -> DoubleX4# -> GraphX20
(&*#) = multGraph
infixl 7 &*#
{-# INLINE (&*#) #-}

toVecX20 :: Double -> GraphX20
toVecX20 = VecX20 . broadcastVector

packX20 :: Graph -> GraphX20
packX20 (Vec g) = VecX20 (generateVector (g U.!))
{-# INLINE packX20 #-}

unpackX20 :: GraphX20 -> Graph
unpackX20 (VecX20 g) = generate (unsafeIndexVector g)
{-# INLINE unpackX20 #-}

genGraphX20 :: Double -> GraphX20
genGraphX20 = generateX20 . const . (/fromIntegral nPatches)

generateX20 :: (Int -> Double) -> GraphX20
generateX20 = VecX20 . generateVector

vecMapX20 :: (Double -> Double) -> GraphX20 -> GraphX20
vecMapX20 f = VecX20 . mapVector f . unVecX20
{-# INLINE vecMapX20 #-}

unsafeVecSumX20 :: GraphX20 -> Double
unsafeVecSumX20 = sumVector . unVecX20
{-# INLINE unsafeVecSumX20 #-}

vecSumX20 :: GraphX20 -> Double
vecSumX20 = vecSum . unpackX20 -- Note: ```sumVector . unVecX20``` yields a different result
{-# INLINE vecSumX20 #-}

genGraph :: Double -> Graph
genGraph = Vec . U.replicate nPatches . (/fromIntegral nPatches)

generate :: (Int -> Double) -> Graph
generate = Vec . U.generate nPatches

vecMapM :: (U.Unbox a, U.Unbox b, Monad f) => (a -> f b) -> Vec a -> f (Vec b)
vecMapM f = fmap Vec . U.mapM f . unVec
{-# INLINE vecMapM #-}

vecMap :: (U.Unbox a, U.Unbox b) => (a -> b) -> Vec a -> Vec b
vecMap f = Vec . U.map f . unVec
{-# INLINE vecMap #-}
{-# SPECIALIZE vecMap :: (Double -> Double) -> Vec Double -> Vec Double #-}

vecImap :: (U.Unbox a, U.Unbox b) => (Int -> a -> b) -> Vec a -> Vec b
vecImap f = Vec . U.imap f . unVec
{-# INLINE vecImap #-}
{-# SPECIALIZE vecImap :: (Int -> Double -> Double) -> Vec Double -> Vec Double #-}

vecSum :: (Num a, U.Unbox a) => Vec a -> a
vecSum = U.sum . unVec
{-# INLINE vecSum #-}
{-# SPECIALIZE vecSum :: Vec Double -> Double #-}

vecMean :: (Fractional a, U.Unbox a) => Vec a -> a
vecMean v = vecSum v / fromIntegral (U.length $ unVec v)
{-# INLINE vecMean #-}
{-# SPECIALIZE vecMean :: Vec Double -> Double #-}

convert :: (U.Unbox a) => Vec a -> V.Vector a
convert = V.convert . unVec

oneAsX20 :: Int -> Double -> GraphX20
oneAsX20 i = packX20 . oneAs i

oneX20 :: Int -> GraphX20
oneX20 = packX20 . one

oneAs :: Int -> Double -> Graph
oneAs i x = Vec $ U.generate nPatches (\j -> if i == j then x else 0.0)

one :: Int -> Graph
one i = Vec $ U.generate nPatches (\j -> if i == j then 1.0 else 0.0)

zero :: Graph
zero = Vec $ U.replicate nPatches 0.0

vecForce :: Graph -> Graph
vecForce (Vec g) = Vec $ U.force g

instance (U.Unbox a, Num a) => Num (Vec a) where
        (+) = (@+@)
        (-) = (@-@)
        (*) = (@*@)
        negate = vecMap negate
        abs = vecMap abs
        signum = vecMap signum
        fromInteger = Vec . U.replicate nPatches . fromInteger
        {-# INLINE (+) #-}
        {-# INLINE (-) #-}
        {-# INLINE (*) #-}
        {-# INLINE negate #-}
        {-# INLINE abs #-}
        {-# INLINE signum #-}
        {-# INLINE fromInteger #-}

(~+~) :: (U.Unbox a, Num a) => Vec (Sum a) -> Vec a -> Vec (Sum a)
Vec x ~+~  Vec y = Vec $ U.zipWith (\v1 v2 -> v1 <> Sum v2) x y
infixl 6 ~+~
{-# INLINE (~+~) #-}
{-# SPECIALIZE (~+~) :: Vec (Sum Double) -> Vec Double -> Vec (Sum Double) #-}

(@+@) :: (U.Unbox a, Num a) => Vec a -> Vec a -> Vec a
Vec x @+@  Vec y = Vec $ U.zipWith (+) x y
infixl 6 @+@
{-# INLINE (@+@) #-}
{-# SPECIALIZE (@+@) :: Vec Double -> Vec Double -> Vec Double #-}

(@-@) :: (U.Unbox a, Num a) => Vec a -> Vec a -> Vec a
Vec x @-@ Vec y = Vec $ U.zipWith (-) x y
infixl 6 @-@
{-# INLINE (@-@) #-}
{-# SPECIALIZE (@-@) :: Vec Double -> Vec Double -> Vec Double #-}

(@*@) :: (U.Unbox a, Num a) => Vec a -> Vec a -> Vec a
Vec x @*@ Vec y = Vec $ U.zipWith (*) x y
infixl 7 @*@
{-# INLINE (@*@) #-}
{-# SPECIALIZE (@*@) :: Vec Double -> Vec Double -> Vec Double #-}

(@/@) :: (U.Unbox a, Fractional a) => Vec a -> Vec a -> Vec a
Vec x @/@ Vec y = Vec $ U.zipWith (/) x y
infixl 7 @/@
{-# INLINE (@/@) #-}
{-# SPECIALIZE (@/@) :: Vec Double -> Vec Double -> Vec Double #-}

(@+) :: (U.Unbox a, Num a) => Vec a -> a -> Vec a
Vec v @+ s = Vec $ U.map (s+) v
infixl 6 @+
{-# INLINE (@+) #-}
{-# SPECIALIZE (@+) :: Vec Double -> Double -> Vec Double #-}

(@-) :: (U.Unbox a, Num a) => Vec a -> a -> Vec a
Vec v @- s = Vec $ U.map (\x -> x-s) v
infixl 6 @-
{-# INLINE (@-) #-}
{-# SPECIALIZE (@-) :: Vec Double -> Double -> Vec Double #-}

(@*) :: (U.Unbox a, Num a) => Vec a -> a -> Vec a
Vec v @* m = Vec $ U.map (m*) v
infixl 7 @*
{-# INLINE (@*) #-}
{-# SPECIALIZE (@*) :: Vec Double -> Double -> Vec Double #-}

(+@) :: (U.Unbox a, Num a) => a -> Vec a -> Vec a
(+@) s = Vec . U.map (s+) . unVec
infixl 6 +@
{-# INLINE (+@) #-}
{-# SPECIALIZE (+@) :: Double -> Vec Double -> Vec Double #-}

(*@) :: (U.Unbox a, Num a) => a -> Vec a -> Vec a
(*@) m = Vec . U.map (m*) . unVec
infixl 7 *@
{-# INLINE (*@) #-}
{-# SPECIALIZE (*@) :: Double -> Vec Double -> Vec Double #-}

(@/) :: (U.Unbox a, Fractional a) => Vec a -> a -> Vec a
Vec q @/ d = Vec $ U.map (/d) q
infixl 7 @/
{-# INLINE (@/) #-}
{-# SPECIALIZE (@/) :: Vec Double -> Double -> Vec Double #-}

