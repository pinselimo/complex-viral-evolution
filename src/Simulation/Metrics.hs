module Simulation.Metrics where

import qualified Data.Matrix       as M (fromRows, toColumns)
import           Data.Vector       (Vector)
import           Statistics.Sample (meanVariance)

reduceSimulation :: (a -> Double) -> [Vector a] -> [(Double, Double)]
reduceSimulation f = map meanVariance . M.toColumns . M.fromRows . map (fmap f)

