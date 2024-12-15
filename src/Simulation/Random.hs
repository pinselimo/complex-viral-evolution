{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Simulation.Random where

import           Control.Monad           (replicateM)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Kind               (Type)
import           Data.Random             (StatefulGen)
import qualified Data.Vector.Unboxed     as U (Vector)
import           Data.Word               (Word32)
import           System.Random.MWC       (Gen, initialize, uniformVector)

-- Source: https://github.com/haskell/mwc-random/issues/39
splitGen :: forall (g :: Type) (m :: Type -> Type). (PrimMonad m, StatefulGen g m)
           => Int -> g -> m [Gen (PrimState m)]
splitGen n gen = genSeeds n gen >>= mkGens

mkGens :: PrimMonad m => [U.Vector Word32] -> m [Gen (PrimState m)]
mkGens = mapM initialize

genSeeds :: forall (g :: Type) (m :: Type -> Type).
              (PrimMonad m, StatefulGen g m) =>
              Int -> g -> m [U.Vector Word32]
genSeeds n gen = replicateM n $ uniformVector gen 256

