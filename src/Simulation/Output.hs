module Simulation.Output where

status :: Int -> Int -> String
status n i = let
         l = 20
         f = round $ (fromIntegral (i+1) / fromIntegral n) * fromIntegral l
         in "\x1B[1A\r["
          <> ['#' | _ <- [1..f]]
          <> [' ' | _ <- [f..l-1]]
          <>  "]   "

