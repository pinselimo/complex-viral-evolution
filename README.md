# SIRMutations - Dynamic mutations in a SEPIARD model

This repository contains the source code for the manuscript _Complex viral evolution as an unintended consequence of social distancing_ that is [currently available as a pre-print](https://doi.org/10.21203/rs.3.rs-5963256/v1).

This model is built using ``nix`` version ``2.24.10`` with ``nix flakes``. To build and run the simulations showing the non-linearity of evolution with a given ``SEED`` on ``CORES`` cores and store the result in the ``data`` directory execute:

~~~
$ nix run '.#hpc' -- -p ./data -s "Just $SEED" +RTS -N"$CORES"
~~~

For a simulation run reproducing the model of Ferguson et al. (2003) run:

~~~
$ nix run '.#repro' -- -p ./data -s "Just $SEED" +RTS -N"$CORES"
~~~

You can change model parameter settings in ``app/HPC.hs`` or ``app/Repro.hs`` respectively.

To optimize performance try adding the following ``RTS`` parameters to the end of the commands: ``-A64m -n2m -qn4``. The ``qn`` value should be no larger than the number of physical cores.

## Without nix

If you don't want to use the ``nix`` build tool you need to install ``llvm``, ``ghc`` and ``cabal`` and refer cabal to a local clone of the ``doublex20`` branch of the following package: (https://github.com/pinselimo/primitive-simd)[https://github.com/pinselimo/primitive-simd]. You can do so by cloning the repository switching to said branch and adding its path to the ``cabal.project.local`` file as: ``packages: /path/to/directory/primitive-simd``.

You can then build and execute the model by running:

~~~
$ cabal run hpc -fvec256 -- -p ./data -s "Just $SEED" +RTS -N"$CORES"
~~~

Or for the reproduction model:

~~~
$ cabal run repro -fvec256 -- -p ./data -s "Just $SEED" +RTS -N"$CORES"
~~~

The model was tested on different x86-64 architecture CPUs on NixOS 24.11 and Ubuntu 22.04.5 LTS, it does not build on Apple Silicon. Simulation times will vary but a single run should usually not exceed an hour of runtime using 4 cores of an i7 CPU. Installation times should be within a couple of minutes.

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

