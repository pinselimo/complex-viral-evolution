module DynamicMutations.SIR where

import           Control.Parallel.Strategies (using, rdeepseq)
import           Data.Foldable           (foldl')
import           Data.Maybe              (fromJust)
import           Data.Tuple.Extra        (uncurry3)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as U (map)

import           DynamicMutations.ODEs       (dd, natBirthRate, natDeathRate, dimm, integrate)
import           DynamicMutations.Parameters (Parameters (nAgents, threshold, cutoffDay, vaccStart, decay, vaccRate, eqTime))
import           DynamicMutations.Types      (Distancing, ImmunityDict, Susceptible,
                                              Variant (..), DeathStore, newDeaths, lastDeaths, Immune)
import           DynamicMutations.Types.PopulationGraph (unVec, Vec(Vec), nPatches, vecMapX20, unpackX20, (&*), unsafeVecSumX20)
import           DynamicMutations.Variant    (calcImmunitiesV, varStep, calcInfectedV, infected, calcBoostedV, varStepNoInf)
import           Simulation.Many             (parVector)

sirStep :: Parameters -> Int -> Distancing -> Susceptible -> Immune -> ImmunityDict -> V.Vector Variant -> DeathStore
        -> (Distancing, Susceptible, Immune, V.Vector Variant, DeathStore)
sirStep ps time dist s sli dict tre ds = let
         dead' = foldl' (\acc v -> acc + dd ps v (symptomaticInfected v) fRec) 0 tre
         distStep lds = time > eqTime ps && time < (eqTime ps + cutoffDay ps) && lds > (threshold ps * (12e6 / fromIntegral nPatches))
         dist' = U.map distStep (unVec (unpackX20 $ lastDeaths ds))
         fRec = vecMapX20 (min 1.0) $ foldl' (\acc v -> acc + recovered v) 0 tre / nAgents ps
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
                              in varStep ps time s sli (boos str) (infs str) fRec dist v (imms (initialized v) str)
                  in (step <$> V.dropWhile noInf tre)
                  `using` parVector ((length tre - length oldTree) `quot` 500) rdeepseq

         tre' = easyTre' <> hardTre'

         s' = s + (if noSLI
                  then foldl' (\acc v -> acc + susceptible' v + immune' v) 0 tre'
                  else foldl' (\acc v -> acc + susceptible' v) 0 tre' + (-sliDif))

         sliDif  = dimm ps $ sli - sum (shortLivedImmuneNon' <$> tre') -- boosted immunities do not recover
         sli' = integrate sliDif sli + foldl' (\acc v -> acc + shortLivedImmuneSus' v) 0 tre'

         (s'', sli'', tre'') = uncurry3 (capTotal ps) $ vaccinate ps time s' sli' tre'

         in ( Vec dist'
            , s''
            , sli''
            , tre''
            , newDeaths ds dead'
            )

vaccinate :: Parameters -> Int -> Susceptible -> Immune -> V.Vector Variant -> (Susceptible, Immune, V.Vector Variant)
vaccinate ps time s sli t
 | time > eqTime ps + cutoffDay ps || time < eqTime ps + cutoffDay ps - vaccStart ps = (s, sli, t)
 | otherwise = case V.uncons t of
        Just (v0, r) -> let
                tvac = time - (eqTime ps + cutoffDay ps - vaccStart ps)
                vac' = nAgents ps &* vaccRate ps
                vacced = (fromIntegral tvac * vac') - sum [vac' * exp (fromIntegral(-ds) * decay ps) | ds <- [1..tvac]]
                overVacc = min 0 $ ss - vacced - vac'
                ss = (s + sli)
                vacces = vac' - overVacc
                crsim = (immune v0 - vacced) / nAgents ps

                vaccedSusceptible = vacces * s/ss
                addedSLI = vaccedSusceptible + decay ps*(vacces * sli/ss)
                in ( s-vaccedSusceptible
                   , sli + addedSLI
                   , V.cons (v0 { shortLivedImmune = shortLivedImmune v0 + vaccedSusceptible * (1-crsim)
                                , boostedCrs = boostedCrs v0 + vaccedSusceptible * crsim
                                }
                            ) r
                   )
        _ -> error "Empty tree cannot exist"

capTotal :: Parameters -> Susceptible -> Immune -> V.Vector Variant -> (Susceptible, Immune, V.Vector Variant)
capTotal ps s sli t = let
         tot = s + (sli + sum(infected <$> t))
         dif = nAgents ps - tot
         f x = natDeathRate ps . x
         ds v = if unsafeVecSumX20 (infected v) == 0
              then v { recovered = f recovered v
                     , shortLivedImmune = f shortLivedImmune v
                     , boostedCrs = f boostedCrs v
                     , mutationPotential = 0
                     }
              else v { exposed = f exposed v
                     , preSymptomatic = f preSymptomatic v
                     , asymptomatic = f asymptomatic v
                     , symptomaticInfected = f symptomaticInfected v
                     , recovered = f recovered v
                     , shortLivedImmune = f shortLivedImmune v
                     , boostedCrs = f boostedCrs v
                     , mutationPotential = f (\v' -> preSymptomatic v' + asymptomatic v' + symptomaticInfected v') v
                     }
         in (natBirthRate ps $ natDeathRate ps $ s + dif, natDeathRate ps sli, ds <$> t)

