module Reproduction.SIR where

import           Control.Parallel.Strategies (using, rdeepseq)
import           Data.Foldable           (foldl')
import           Data.Maybe              (fromJust)
import qualified Data.Vector             as V

import           DynamicMutations.Types  (ImmunityDict, Susceptible, Immune)
import           DynamicMutations.Types.PopulationGraph (unsafeVecSumX20)
import           Reproduction.ODEs       (natBirthRate, natDeathRate, integrate, dimm)
import           Reproduction.Parameters (Parameters (nAgents, decay))
import           Reproduction.Types      (Variant (..))
import           Reproduction.Variant    (calcImmunitiesV, infectionStep, calcInfectedV, infected, calcBoostedV, varStepNoInf)
import           Simulation.Many         (parVector)

sirStep :: Parameters -> Int -> Susceptible -> Immune -> ImmunityDict -> V.Vector Variant
        -> (Susceptible, Immune, V.Vector Variant)
sirStep ps time s sli dict tre = let
         imms = calcImmunitiesV dict tre
         infs = calcInfectedV dict tre
         boos = calcBoostedV dict tre
         noSLI = decay ps > 1.0

         noInf = (==0) . unsafeVecSumX20 . infected
         oldTree = V.takeWhile noInf tre
         easyTre' = (varStepNoInf ps <$> oldTree)
                  `using` parVector (length oldTree `quot` 100) rdeepseq
         hardTre' = let step v = if unsafeVecSumX20 (infected v) == 0 then varStepNoInf ps v
                           else let str = fromJust (astrain v)
                                in infectionStep ps time s sli (boos str) (infs str) v (imms (initialized v) str)
                  in (step <$> V.dropWhile noInf tre)
                  `using` parVector ((length tre - length oldTree) `quot` 500) rdeepseq

         tre' = easyTre' <> hardTre'

         s' = s + (if noSLI
                  then foldl' (\acc v -> acc + susceptible' v + immune' v) 0 tre'
                  else foldl' (\acc v -> acc + susceptible' v) 0 tre' + (-sli'))

         sli'  = dimm ps $ sli - sum (shortLivedImmuneNon' <$> tre') -- boosted immunities do not recover
         sli'' = integrate sli' sli + foldl' (\acc v -> acc + shortLivedImmuneSus' v) 0 tre'

         in capTotal ps s' sli'' tre'

capTotal :: Parameters -> Susceptible -> Immune -> V.Vector Variant -> (Susceptible, Immune, V.Vector Variant)
capTotal ps s sli t = let
         tot = s + (sli + sum(infected <$> t))
         dif = nAgents ps - tot
         f x v = natDeathRate ps (x v)
         ds v = if unsafeVecSumX20 (infected v) == 0
              then v { recovered = f recovered v
                     , shortLivedImmune = f shortLivedImmune v
                     , boostedCrs = f boostedCrs v
                     , mutationPotential = 0
                     }
              else v { exposed = f exposed v
                     , symptomaticInfected = f symptomaticInfected v
                     , recovered = f recovered v
                     , shortLivedImmune = f shortLivedImmune v
                     , boostedCrs = f boostedCrs v
                  , mutationPotential = f symptomaticInfected v
                  }
         in (natBirthRate ps $ natDeathRate ps $ s + dif, natDeathRate ps sli, ds <$> t)

