{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
module Simulation (module Simulation, module Simulation.Many)
 where

import           Control.Arrow         (Arrow (second))
import           Control.Concurrent    (newChan, readChan, writeChan, takeMVar, putMVar, newEmptyMVar)
import           Control.Concurrent.Async (async, waitCatch)
import           Control.Monad         (forever, void, when)
import           Control.Monad.ST      (runST)
import           Control.Monad.Trans   (MonadTrans, lift)
import           Data.Functor.Identity (Identity (Identity))
import           System.Random.MWC     (initialize)

import           Simulation.Many
import           Simulation.Random     (genSeeds)

simulateTimedReps_ :: Monad m => Int -> (Int -> a -> m a) -> a -> m ()
simulateTimedReps_ n a = go 0
        where go i x = if i >= n
                       then return ()
                       else do
                        !x' <- a i x
                        go (i+1) x'

simulateWriteTimedReps1 :: (MonadTrans t, Monad (t IO)) => (Int -> a -> IO ()) -> Int -> (Int -> a -> t IO a) -> a -> t IO a
simulateWriteTimedReps1 w n a start = do
        chan <- lift newChan
        sync <- lift newEmptyMVar
        h <- lift (async (writer sync chan))
        go h sync chan 0 start
        where go handle sync chan i x = do
                       _  <- lift (writeChan chan (i, x))
                       x' <- a i x
                       when (i `mod` 10 == 0) (void $ lift (takeMVar sync))
                       if i >= n
                       then lift (waitCatch handle) >> return x
                       else go handle sync chan (i+1) x'
              writer sync chan = forever $ do
                (i, x) <- readChan chan
                w i x
                when (i `mod` 10 == 0) (putMVar sync ())

simulateTimedReps1 :: (MonadTrans t, Monad (t IO)) => Int -> (Int -> a -> t IO a) -> a -> t IO a
simulateTimedReps1 = simulateWriteTimedReps1 (\_ _ -> return ())

simulateTimedRepsC :: Monad m => (Int -> a -> b) -> Int -> Int -> (Int -> a -> m a) -> a -> m [b]
simulateTimedRepsC comp n nCol a = go 0
        where go i x = let
                !c = comp i x
                in if i >= n
                       then return []
                       else do
                        x' <- a i x
                        xs <- go (i+1) x'
                        return $! if (i`mod`nCol) == 0 then c : xs else xs

simulateTimedRepsC1 :: Monad m => (Int -> a -> b) -> Int -> Int -> (Int -> a -> m a) -> a -> m (a, [b])
simulateTimedRepsC1 comp n nCol a = go 0
        where go i x = let
                !c = comp i x
                in if i >= n
                       then return (x, [])
                       else do
                        x' <- a i x
                        xs <- go (i+1) x'
                        return $! if (i`mod`nCol) == 0 then second (c :) xs else xs

simulateTimedReps :: Monad m => Int -> (Int -> a -> m a) -> a -> m [a]
simulateTimedReps n = go 0
        where go i a x = (x:) <$> if i >= n
                       then return []
                       else a i x >>= go (i+1) a

simulateReps_ :: Monad m => Int -> (a -> m a) -> a -> m ()
simulateReps_ n = simulateTimedReps_ n . const

simulateReps1 :: (MonadTrans t, Monad (t IO)) => Int -> (a -> t IO a) -> a -> t IO a
simulateReps1 n = simulateTimedReps1 n . const

simulateRepsC :: Monad m => Int -> Int -> (a -> b) -> (a -> m a) -> a -> m [b]
simulateRepsC n nCol f = simulateTimedRepsC (const f) n nCol . const

simulateRepsC1 :: Monad m => Int -> Int -> (a -> b) -> (a -> m a) -> a -> m (a, [b])
simulateRepsC1 n nCol f = simulateTimedRepsC1 (const f) n nCol . const

simulateReps :: Monad m => Int -> (a -> m a) -> a -> m [a]
simulateReps n = simulateTimedReps n . const

simulateOne :: Simulation Identity a r
simulateOne initial seed sim = Identity $ runST (initialize seed >>= sim initial)

simulateOneIO :: SimulationN Identity a r
simulateOneIO initial seed sim = let
           seeds = runST (initialize seed >>= genSeeds 1)
        in mapM (\(i, sd) -> initialize sd >>= sim i initial) $ Identity (1, head seeds)
