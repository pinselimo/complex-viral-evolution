{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
module Simulation.Many where

import           Control.Concurrent.ParallelIO (parallel)
import           Control.Monad               (replicateM)
import           Control.Monad.ST            (runST)
import           Control.Monad.Trans.Random.Strict (RandT)
import           Control.Parallel.Strategies (NFData, parMap, rdeepseq
                                             ,parTraversable, Strategy, parList, evalTraversable)
import           Data.List                   (unfoldr)
import           Data.Random                 (StatefulGen)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U (Vector)
import           Data.Word                   (Word32)
import           System.Random.MWC           (initialize)

import           Simulation.Random           (genSeeds)

type Simulation f a r = Foldable f
                        => a -> U.Vector Word32
                        -> (forall g m. (Monad m, StatefulGen g (RandT g m))
                            => a -> g -> m r
                           )
                        -> f r

type SimulationN f a r = Foldable f
                        => a -> U.Vector Word32
                        -> (forall g. (StatefulGen g (RandT g IO))
                            => Int -> a -> g -> IO r
                           )
                        -> IO (f r)

simulateParallel :: NFData r => Int -> Simulation [] a r
simulateParallel n initial seed sim = let
           seeds = runST (initialize seed >>= genSeeds n)
        in parMap rdeepseq (\sd -> runST (initialize sd >>= sim initial)) seeds

simulateParallelN :: NFData r => Int -> SimulationN [] a r
simulateParallelN n initial seed sim = let
           seeds = runST (initialize seed >>= genSeeds n)
        in parallel $ (\(i, sd) -> initialize sd >>= sim i initial) <$> zip [1..] seeds

simulateSequential :: Int -> Simulation [] a r
simulateSequential n initial seed sim = runST (initialize seed >>= replicateM n . sim initial)

parVector :: Int -> Strategy a -> Strategy (V.Vector a)
parVector n strat v
  | n <= 1 = parTraversable strat v
  | otherwise = V.concat <$> parList (evalTraversable strat) (chunksOf n v)

chunksOf :: Int -> V.Vector a -> [V.Vector a]
chunksOf i = unfoldr go
  where go v | V.null v = Nothing
             | otherwise = Just (V.splitAt i v)

