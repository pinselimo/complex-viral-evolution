{-# LANGUAGE NumericUnderscores #-}
module Reproduction.Parameters where

import qualified DynamicMutations.Immunity as Immunity (MutationParameters(..))
import DynamicMutations.Types.PopulationGraph (GraphX20, oneAsX20, genGraphX20, generateX20, nPatches, toVecX20)

data Parameters = Parameters {
        nAgents          :: GraphX20
      , nInit            :: GraphX20
      , mutability       :: Double
      , decay            :: GraphX20 -- Decay of short lived immunity
      , escapeThreshold  :: Int
      , maximalLevelCI   :: Double
      , minimalLevelCT   :: Double
      , nEpitopes        :: Int
      , nCodons          :: Int
      , eqTime           :: Int
      , lifespan         :: Int -- days
      , epsilon          :: GraphX20
      , homophily        :: GraphX20

      -- Variant parameters
      , initα            :: Double -- average latent period of 4 days
      , initβ            :: Double -- = infectiousness * nContacts -- average number of infectious contacts
      , initγ            :: Double -- Average 4 days of a(-symptomatic) period
      } deriving (Show)

instance Immunity.MutationParameters Parameters where
        escapeThreshold = escapeThreshold
        maximalLevelCI = maximalLevelCI
        minimalLevelCT = minimalLevelCT
        nEpitopes = nEpitopes
        nCodons = nCodons

standardParameters :: Parameters
standardParameters = Parameters
                { nAgents = genGraphX20 12e6
                , nInit = oneAsX20 0 1
                , mutability = 2.5e-6
                , decay = toVecX20 $ 1/270
                , escapeThreshold = 2
                , maximalLevelCI = 0.99
                , minimalLevelCT = 0.15
                , nEpitopes = 4
                , nCodons = 3
                , eqTime = 0
                , lifespan = 30*365
                , epsilon = generateX20 (\i -> if i < (nPatches `div` 2) then 0.25 else -0.25)
                , homophily = toVecX20 $ 249/269

                -- Variant parameters
                , initα = 0.5
                , initβ = 269/200
                , initγ = 0.25
                }
