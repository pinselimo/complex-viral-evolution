module DynamicMutations.Immunity where

import           Control.Monad.State.Strict (State, get, put)
import           Data.Foldable           (Foldable(foldMap'))
import qualified Data.HashMap.Strict     as Map (HashMap, fromList, lookup, map, mapKeys)
import qualified Data.HashSet            as Set (fromList)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromJust)
import           Data.Monoid             (Sum(Sum, getSum))
import           Data.Random             (StatefulGen, sampleFrom, uniform)
import qualified Data.Sequence           as Seq
import qualified Data.Vector             as V
import qualified Data.Vector.Storable    as VS
import qualified Data.Vector.Unboxed     as U
import           Data.Word               (Word8)

import           DynamicMutations.Types      (AminoAcid (..), AminoStrain,
                                              Base (..), Codon, ImmunityDict,
                                              CICache, Strain, codonW8, hsh, EffStrain)

class MutationParameters ps where
        escapeThreshold :: ps -> Int
        maximalLevelCI  :: ps -> Double
        minimalLevelCT  :: ps -> Double
        nCodons   :: ps -> Int
        nEpitopes :: ps -> Int

bases :: V.Vector Base
bases = V.force (V.fromList [U, C, A, G])

countEq :: AminoStrain -> AminoStrain -> Int
countEq a b = U.length $ U.filter id $ U.zipWith (/=) a b
{-# INLINE countEq #-}

immunity :: MutationParameters ps => ps -> AminoStrain -> AminoStrain -> Double
immunity ps a b
  | c == 0 = 1
  | c >= escapeThreshold ps
  = maximalLevelCI ps
      + (minimalLevelCT ps - maximalLevelCI ps)
          * fromIntegral (c - escapeThreshold ps)
          / fromIntegral (nEpitopes ps * nCodons ps - escapeThreshold ps)
  | otherwise = maximalLevelCI ps
  where c = countEq a b

-- Assuumes that a > b
unsafeFastImmunityUp :: CICache -> Int -> Int -> Double
unsafeFastImmunityUp iim a b = (iim `V.unsafeIndex` a) `VS.unsafeIndex` b
{-# INLINE unsafeFastImmunityUp #-}

fastImmunity :: CICache -> Int -> Int -> Double
fastImmunity iim a b = if a == b then 1.0  else (iim V.! max a b) VS.! min a b
{-# INLINE fastImmunity #-}

update :: MutationParameters ps => ps -> ImmunityDict -> AminoStrain -> ImmunityDict
update ps (as, is, iim) s = let
        nNew = V.length is
        iim' = iim `V.snoc` VS.convert ((immunity ps s <$> is) `V.snoc` 1.0)
        in (M.insert s nNew as, is `V.snoc` s, iim')

register :: MutationParameters ps => ps -> AminoStrain -> State (Int, ImmunityDict) (Int, Int)
register ps s = do
        (ix, d@(as, _, _)) <- get
        case as M.!? s of
          Just i -> put (ix+1, d) >> return (ix, i)
          Nothing -> do
           let d'@(_, _, iim) = update ps d s
           put (ix+1, d')
           return (ix, V.length iim - 1)

strainToAmino :: Strain -> Seq.Seq AminoAcid
strainToAmino = fmap (fromJust . codonToAmino)

(<?>) :: Eq a => a -> a -> Int
(<?>) a b = if a == b then 0 else 1
infixr 9 <?>
{-# INLINE (<?>) #-}
{-# SPECIALIZE (<?>) :: Base -> Base -> Int #-}

nucDiff :: (Strain, Double) -> (Strain, Double) -> Double
nucDiff (s1, w1) (s2, w2) = w1 * w2 * fromIntegral (getSum $ foldMap' Sum (Seq.zipWith diff s1 s2))
        where diff (a, b, c) (x, y, z) = a <?> x + b <?> y + c <?> z
{-# INLINE nucDiff #-}

effStrainToHshAmino :: EffStrain -> AminoStrain
effStrainToHshAmino = U.map (fromJust . (`Map.lookup` hsh8))

hsh8 :: Map.HashMap Word8 Word8
hsh8 = Map.map hsh (Map.mapKeys codonW8 codonAminoMap)

codonAminoMap :: Map.HashMap Codon AminoAcid
codonAminoMap = Map.fromList
        [ ((U, U, U), Phenylalanine)
        , ((U, U, C), Phenylalanine)
        , ((U, U, A), Leucine)
        , ((U, U, G), Leucine)
        , ((U, C, U), Serine)
        , ((U, C, C), Serine)
        , ((U, C, A), Serine)
        , ((U, C, G), Serine)
        , ((U, A, U), Tyrosine)
        , ((U, A, C), Tyrosine)
        -- , ((U, A, A), Stop)
        -- , ((U, A, G), Stop)
        , ((U, G, U), Cysteine)
        , ((U, G, C), Cysteine)
        -- , ((U, G, A), Stop)
        , ((U, G, G), Tryptophan)
        , ((C, U, U), Leucine)
        , ((C, U, C), Leucine)
        , ((C, U, A), Leucine)
        , ((C, U, G), Leucine)
        , ((C, C, U), Proline)
        , ((C, C, C), Proline)
        , ((C, C, A), Proline)
        , ((C, C, G), Proline)
        , ((C, A, U), Histidine)
        , ((C, A, C), Histidine)
        , ((C, A, A), Glutamine)
        , ((C, A, G), Glutamine)
        , ((C, G, U), Arginine)
        , ((C, G, C), Arginine)
        , ((C, G, A), Arginine)
        , ((C, G, G), Arginine)
        , ((A, U, U), Isoleucine)
        , ((A, U, C), Isoleucine)
        , ((A, U, A), Isoleucine)
        , ((A, U, G), Methionine)
        , ((A, C, U), Threonine)
        , ((A, C, C), Threonine)
        , ((A, C, A), Threonine)
        , ((A, C, G), Threonine)
        , ((A, A, U), Asparagine)
        , ((A, A, C), Asparagine)
        , ((A, A, A), Lysine)
        , ((A, A, G), Lysine)
        , ((A, G, U), Serine)
        , ((A, G, C), Serine)
        , ((A, G, A), Arginine)
        , ((A, G, G), Arginine)
        , ((G, U, U), Valine)
        , ((G, U, C), Valine)
        , ((G, U, A), Valine)
        , ((G, U, G), Valine)
        , ((G, C, U), Alanine)
        , ((G, C, C), Alanine)
        , ((G, C, A), Alanine)
        , ((G, C, G), Alanine)
        , ((G, A, U), AsparticAcid)
        , ((G, A, C), AsparticAcid)
        , ((G, A, A), GlutamicAcid)
        , ((G, A, G), GlutamicAcid)
        , ((G, G, U), Glycine)
        , ((G, G, C), Glycine)
        , ((G, G, A), Glycine)
        , ((G, G, G), Glycine)
        ]

codonToAmino :: Codon -> Maybe AminoAcid
codonToAmino = (`Map.lookup` codonAminoMap)

_codonToAmino :: Codon -> Maybe AminoAcid
_codonToAmino (a, b, c) = case a of
        U -> case b of
                U -> Just $ case c of
                        U -> Phenylalanine
                        C -> Phenylalanine
                        _ -> Leucine
                C -> Just Serine
                A -> case c of
                        U -> Just Tyrosine
                        C -> Just Tyrosine
                        _ -> Nothing -- Stop codon
                G -> case c of
                        A -> Nothing -- Stop codon
                        G -> Just Tryptophan
                        _ -> Just Cysteine
        C -> Just $ case b of
                U -> Leucine
                C -> Proline
                A -> case c of
                        U -> Histidine
                        C -> Histidine
                        _ -> Glutamine
                G -> Arginine
        A -> Just $ case b of
                U -> case c of
                        G -> Methionine
                        _ -> Isoleucine
                C -> Threonine
                A -> case c of
                        U -> Asparagine
                        C -> Asparagine
                        _ -> Lysine
                G -> case c of
                        U -> Serine
                        C -> Serine
                        _ -> Arginine
        G -> Just $ case b of
                U -> Valine
                C -> Alanine
                A -> case c of
                        U -> AsparticAcid
                        C -> AsparticAcid
                        _ -> GlutamicAcid
                G -> Glycine

randomBase :: StatefulGen g m => g -> m Base
randomBase gen = do
                i <- sampleFrom gen (uniform 0 3)
                return $! bases V.! i

randomCodon :: StatefulGen g m => g -> m Codon
randomCodon g = do
        a <- randomBase g
        b <- randomBase g
        c <- randomBase g
        let cod = (a, b, c)
        if isStop cod
        then randomCodon g
        else return $! cod

randomStrain :: (MutationParameters ps, StatefulGen g m) => ps -> g -> m Strain
randomStrain ps = Seq.replicateM (nEpitopes ps * nCodons ps) . randomCodon

isStop :: Codon -> Bool
isStop = (`elem` Set.fromList [(U, A, A), (U, A, G), (U, G, A)]) -- stop codons

