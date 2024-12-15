{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE DataKinds      #-}
module DynamicMutations.Types where

import           Control.DeepSeq     (NFData)
import           Data.Bits           ((.<<.), (.>>.), (.&.), (.|.), (.^.))
import           Data.Hashable       (Hashable)
import qualified Data.Map.Strict     as M
import           Data.Primitive.SIMD (SIMDVector(nullVector))
import qualified Data.Sequence       as Seq
import qualified Data.Vector         as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           Data.Word           (Word8)
import           GHC.Generics        (Generic)

import           DynamicMutations.Types.PopulationGraph (Vec, GraphX20(VecX20), unpackX20, vecSum)
import           DynamicMutations.Types.Tree (PhyloTree)

type Susceptible = GraphX20
type Immune      = GraphX20
type Distancing  = Vec Bool
type Codon = (Base, Base, Base)
type Strain = Seq.Seq Codon
type Diversity = [Int]
type AminoStrain = U.Vector Word8
type EffStrain = U.Vector Word8
type CICache = V.Vector (VS.Vector Double)
type DiffCache = V.Vector (VS.Vector Word8)
type ImmunityDict = (M.Map AminoStrain Int, V.Vector AminoStrain, CICache)
type DiffDict = (V.Vector EffStrain, DiffCache)
type ModelState = (Distancing, Susceptible, Immune, ImmunityDict, V.Vector Variant, PhyloTree Int, DeathStore)

strHsh :: Seq.Seq Codon -> EffStrain
strHsh = let
  strHsh' (cod Seq.:<| r) = codonW8 cod : strHsh' r
  strHsh' Seq.Empty = []
        in U.fromList . strHsh'
{-# INLINE strHsh #-}

strUnHsh :: EffStrain -> Seq.Seq Codon
strUnHsh = let
  strUnHsh' (cod : r) = w8Codon cod Seq.:<| strUnHsh' r
  strUnHsh' [] = Seq.Empty
        in strUnHsh' . U.toList
{-# INLINE strUnHsh #-}

codonW8 :: Codon -> Word8
codonW8 (a, b, c) = let
        a', b', c' :: Word8
        a' = toEnum (fromEnum a)
        b' = toEnum (fromEnum b)
        c' = toEnum (fromEnum c)
        in a' .<<. 4 .|. b' .<<. 2 .|. c'
{-# INLINE  codonW8 #-}

w8Codon :: Word8 -> Codon
w8Codon w = let
        c = toEnum . fromEnum
        in (c (w .>>. 4 .&. 3), c (w .>>. 2 .&. 3), c (w .&. 3))
{-# INLINE  w8Codon #-}

compCodonW8 :: Word8 -> Word8 -> Word8
compCodonW8 a b = let
        cmp = a .^. b
        get x = let x' = x.&.3 in x' .&. 1 .|. x' .>>. 1
        in get cmp + get (cmp .>>. 2) + get (cmp .>>. 4)
{-# INLINE compCodonW8 #-}

data Sel = X | Y | Z
data Base = U | C | A | G deriving (Show, Eq, Generic, NFData, Hashable, Enum)

data AminoAcid = Phenylalanine
               | Leucine
               | Isoleucine
               | Methionine
               | Valine
               | Serine
               | Proline
               | Threonine
               | Alanine
               | Tyrosine
               | Histidine
               | Glutamine
               | Asparagine
               | Lysine
               | AsparticAcid
               | GlutamicAcid
               | Cysteine
               | Tryptophan
               | Arginine
               | Glycine
               deriving (Show, Eq, Generic, NFData, Enum)

hsh :: AminoAcid -> Word8
hsh = toEnum . fromEnum

data Variant = Variant {
          α                   :: {-# UNPACK #-} !Double
        , β                   :: {-# UNPACK #-} !Double
        , γ                   :: {-# UNPACK #-} !Double
        , λ                   :: {-# UNPACK #-} !Double
        , µ                   :: {-# UNPACK #-} !Double
        , ν                   :: {-# UNPACK #-} !Double
        , susceptible'        :: !GraphX20
        , immune'             :: !GraphX20
        , exposed             :: !GraphX20
        , preSymptomatic      :: !GraphX20
        , asymptomatic        :: !GraphX20
        , symptomaticInfected :: !GraphX20
        , shortLivedImmune    :: !GraphX20
        , recovered           :: !GraphX20
        , immune              :: !GraphX20
        , dead                :: !GraphX20
        , mutationPotential   :: !GraphX20
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
         ++ ", λ = " ++ show (λ v) ++ ", µ = " ++ show (µ v) ++ ", ν = " ++ show (ν v)
         , "susceptible': " ++ showv (susceptible' v)
         , "immune': " ++ showv (immune' v)
         , "exposed " ++showv (exposed v)
         , "preSymptomatic " ++showv (preSymptomatic v)
         , "asymptomatic " ++showv (asymptomatic v)
         , "symptomaticInfected " ++showv (symptomaticInfected v)
         , "recovered " ++showv (recovered v)
         , "immune " ++showv (immune v)
         , "dead " ++showv (dead v)
         , "mutationPotential " ++showv (mutationPotential v)
         , "mid " ++show (mid v)
         , "strain: " ++ show (strain v) ++ " with #" ++ show (astrain v)
         , "initialized: " ++ show (initialized v)
         ]

data Result = Result {
          distancingPatches     :: {-# UNPACK #-} !Int
        , totalSusceptible      :: {-# UNPACK #-} !Double
        , totalShortLivedImmune :: {-# UNPACK #-} !Double
        , linearity             :: {-# UNPACK #-} !Double
        , janssonEntropy        :: {-# UNPACK #-} !Double
        , janssonWeighted       :: {-# UNPACK #-} !Double
        , totalInfected         :: {-# UNPACK #-} !Double
        , totalRecovered        :: {-# UNPACK #-} !Double
        , totalDead             :: {-# UNPACK #-} !Double
        , treeSize              :: {-# UNPACK #-} !Int
        , totalIncidence        :: {-# UNPACK #-} !Double
        , pairwiseDiversity     :: {-# UNPACK #-} !Double
        , patch1Incidence       :: {-# UNPACK #-} !Double
        , boostedImmunities     :: {-# UNPACK #-} !Double
        , unweightedLinearity   :: {-# UNPACK #-} !Double
        , meanCrossImmunity     :: {-# UNPACK #-} !Double
        , meanBeta              :: {-# UNPACK #-} !Double
        , meanLambda            :: {-# UNPACK #-} !Double
        , meanAlpha             :: {-# UNPACK #-} !Double
        , meanGamma             :: {-# UNPACK #-} !Double
        , meanMu                :: {-# UNPACK #-} !Double
        , meanNu                :: {-# UNPACK #-} !Double
        , allenEntropy          :: {-# UNPACK #-} !Double
        }
        deriving (Show, Generic, NFData, Eq)

data DeathStore = DeathStore { pos :: {-# UNPACK #-} !Int, graph :: U.Vector GraphX20 }
        deriving (Generic, NFData)

initDeaths :: DeathStore
initDeaths = DeathStore 0 $ U.replicate 10 (VecX20 nullVector)

lastDeaths :: DeathStore -> GraphX20
lastDeaths (DeathStore _ gs) = U.foldl' (+) 0 gs

newDeaths :: DeathStore -> GraphX20 -> DeathStore
newDeaths (DeathStore p gs) g = let p' = (p+1) `mod` 10 in DeathStore p' (U.modify (\v -> MU.write v p g) gs)

