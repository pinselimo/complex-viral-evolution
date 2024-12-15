{-# LANGUAGE NumericUnderscores #-}
module DynamicMutations.Parameters where

import qualified DynamicMutations.Immunity as Immunity (MutationParameters(..))
import DynamicMutations.Types.PopulationGraph (GraphX20, oneAsX20, genGraphX20, generateX20, nPatches, toVecX20)

data Parameters = Parameters {
        nAgents          :: GraphX20
      , nInit            :: GraphX20
      , mutability       :: Double
      , σI               :: Double
      , ψII              :: GraphX20 -- cross protection
      , decay            :: GraphX20 -- Decay of short lived immunity
      , escapeThreshold  :: Int
      , maximalLevelCI   :: Double
      , minimalLevelCT   :: Double
      , nEpitopes        :: Int
      , nCodons          :: Int
      , threshold        :: Double -- Threshold for social distancing
      , distancingEffect :: Double -- effect of social distancing
      , eqTime           :: Int
      , lifespan         :: Int -- days
      , epsilon          :: GraphX20
      , homophily        :: GraphX20
      , cutoffDay        :: Int
      , vaccRate         :: Double -- fraction of inhabitants per day
      , vaccStart        :: Int -- days before cutoffDay

      -- Variant parameters
      , initα            :: Double -- average latent period of 4 days
      , initβ            :: Double -- = infectiousness * nContacts -- average number of infectious contacts
      , initγ            :: Double -- Average 4 days of a(-symptomatic) period
      , initλ            :: Double -- Chance to survive
      , initµ            :: Double -- Average 2 days of presymptomatic period
      , initν            :: Double -- share of symptomatic infections
      } deriving (Show)

instance Immunity.MutationParameters Parameters where
        escapeThreshold = escapeThreshold
        maximalLevelCI = maximalLevelCI
        minimalLevelCT = minimalLevelCT
        nEpitopes = nEpitopes
        nCodons = nCodons

meanMutability :: Int -> Double -> Double
meanMutability days m = let d = fromIntegral days in sum (map (*m) [1..d]) / d

standardParameters :: Parameters
standardParameters = Parameters
                { nAgents = genGraphX20 12e6
                , nInit = oneAsX20 0 1
                , mutability = 2.5e-6
                , σI = 0.05
                , ψII = toVecX20 0.99
                , decay = toVecX20 $ 1/(270-6)
                , escapeThreshold = 2
                , maximalLevelCI = 0.99
                , minimalLevelCT = 0.15
                , nEpitopes = 4
                , nCodons = 3
                , threshold = 1e-4
                , distancingEffect = 0.5
                , eqTime = 0
                , lifespan = 30*365
                , epsilon = generateX20 (\i -> if i < (nPatches `div` 2) then 0.25 else -0.25)
                , homophily = toVecX20 $ 269/289
                , cutoffDay = 4*365
                , vaccRate  = 0/100
                , vaccStart = 100

                -- Variant parameters
                , initα = 0.25
                , initβ = 289/200
                , initγ = 0.5
                , initλ = 0.99
                , initµ = 0.5
                , initν = 0.7
                }
