{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE DataKinds      #-}
module Reproduction.Types where

import           Control.DeepSeq     (NFData)
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)

import           DynamicMutations.Types (Susceptible, Immune, ImmunityDict, EffStrain)
import           DynamicMutations.Types.PopulationGraph (GraphX20, unpackX20, vecSum)
import           DynamicMutations.Types.Tree (PhyloTree)

type ModelState = (Susceptible, Immune, ImmunityDict, V.Vector Variant, PhyloTree Int)

data Variant = Variant {
          α                   :: {-# UNPACK #-} !Double
        , β                   :: {-# UNPACK #-} !Double
        , γ                   :: {-# UNPACK #-} !Double
        , susceptible'        :: !GraphX20
        , immune'             :: !GraphX20
        , exposed             :: !GraphX20
        , symptomaticInfected :: !GraphX20
        , recovered           :: !GraphX20
        , immune              :: !GraphX20
        , mutationPotential   :: !GraphX20
        , shortLivedImmune    :: !GraphX20
        , shortLivedImmuneSus' :: !GraphX20
        , shortLivedImmuneNon' :: !GraphX20
        , incidence           :: !GraphX20
        , boostedCrs          :: !GraphX20
        , mid                 :: Maybe (Int, Int)
        , strain              :: !EffStrain
        , astrain             :: Maybe Int
        , initialized         :: Bool
        }
        deriving (Generic, NFData, Eq)

instance Show Variant where
        show v = let
                 showv vec = let vec' = unpackX20 vec in show vec' ++ " with sum: " ++ show (vecSum vec')
                 in unlines [
           "α = " ++ show (α v) ++ ", β = " ++ show (β v) ++ ", γ = " ++ show (γ v)
         , "susceptible': " ++ showv (susceptible' v)
         , "immune': " ++ showv (immune' v)
         , "exposed " ++showv (exposed v)
         , "symptomaticInfected " ++showv (symptomaticInfected v)
         , "recovered " ++showv (recovered v)
         , "immune " ++showv (immune v)
         , "mutationPotential " ++showv (mutationPotential v)
         , "mid " ++show (mid v)
         , "strain: " ++ show (strain v) ++ " with #" ++ show (astrain v)
         , "initialized: " ++ show (initialized v)
         ]

