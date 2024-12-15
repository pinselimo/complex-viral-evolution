# SIRMutations - Dynamic mutations in a SEPIARD model

This model is built using ``nix`` ``flakes``. To build and run the simulations showing the non-linearity of evolution with a given ``SEED`` on ``CORES`` cores and store the result in the ``data`` directory execute:

~~~
$ nix run '.#hpc' -- -p ./data -s "Just $SEED" +RTS -N"$CORES"
~~~

For a simulation run reproducing the model of Ferguson et al. (2003) run:

~~~
$ nix run '.#repro' -- -p ./data -s "Just $SEED" +RTS -N"$CORES"
~~~

You can change model parameter settings in ``app/HPC.hs`` or ``app/Repro.hs`` respectively.

To optimize performance try adding the following ``RTS`` parameters to the end of the commands: ``-A64m -n2m -qn4``. The ``qn`` value should be no larger than the number of physical cores.

## Generated Data

The generated ``csv`` Files contain keys that relate to the stored values as listed below:


    0  : Non-pharmaceutical intervention prevalence
    1  : Susceptible population
    2  : Non-specific immune population
    3  : Linearity metric (weighted)
    4  : Jansson et al. (2012)'s entropy metric ($H^* $)
    5  : Weighted variant of Jansson et al. (2012)'s entropy ($H_w^* $)
    6  : Infected population
    7  : Recovered population
    8  : Dead population
    9  : Phylogenetic tree size
    10 : Global incidence
    11 : Pairwise nucleotide diversity
    12 : Incidence in the first patch
    13 : Boosted immune population
    14 : Weighted mean cross-immunity population per variant
    15 : Unweighted linearity metric
    16 : Weighted mean of $\beta$ parameter
    17 : Weighted mean of $\lambda$ parameter
    18 : Weighted mean of $\alpha$ parameter
    19 : Weighted mean of $\gamma$ parameter
    20 : Weighted mean of $\mu$ parameter
    21 : Weighted mean of $\nu$ parameter
    22 : Allen et al. (2009)'s entropy metric ($H_p$)


Note that the reproduction model does not record all this data as it has no interventions, no death rate and no use for the parameters $\lambda$, $\mu$ and $\nu$.


## References

Ferguson, Neil M., Alison P. Galvani, and Robin M. Bush. 2003. “Ecological and Immunological Determinants of Influenza Evolution.” Nature 422 (6930): 428–33. https://doi.org/10.1038/nature01509.

Jansson, Jesper, Kunihiko Sadakane, and Wing-Kin Sung. 2012. “Ultra-Succinct Representation of Ordered Trees with Applications.” Journal of Computer and System Sciences, Games in Verification, 78 (2): 619–31. https://doi.org/10.1016/j.jcss.2011.09.002.

Allen, Benjamin, Mark Kon, and Yaneer Bar‐Yam. 2009. “A New Phylogenetic Diversity Measure Generalizing the Shannon Index and Its Application to Phyllostomid Bats.” The American Naturalist 174 (2): 236–43. https://doi.org/10.1086/600101.

