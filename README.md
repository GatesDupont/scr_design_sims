# Simulation code
### From: *"Towards optimal sampling design for spatial capture-recapture"*


This repository contains the code to generate and evaluate SCR sampling designs that were generated using the framework presented in the abovementioned manudscript. The R code, found in the R subdirectory, operates within the file structure of this repository. Simulations are performed in the `6_sims.r` file, which pulls other files from the directory that are conveniently compiled into the R data file: `workspace/sims_ws.RData`. The simulations file is currently parameterized to run all simulations simultaneously, and for sake of efficiency, the simulations are distributed across several cores.

Sampling designs evaluated here were generated using the function `scrdesignGA` from the R package [`oSCR`](https://sites.google.com/site/spatialcapturerecapture/oscr-package) [(Sutherland et al., 2019)](https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.04551). You can download oSCR from GitHub using the following code:

```r
remotes::install_github("jaroyle/oSCR")
```

The help file for `scrdesignGA` can be accessed through the package, using: `?scrdesignGA`, and is also available [online](https://rdrr.io/github/jaroyle/oSCR/man/scrdesignGA.html). Included in the help file is an example of how to generate designs from the Astore region of Pakistan, which was used here.

Simulations were completed using R version 3.6.1 (R Core Team, 2019).

1. Sutherland, C., Royle, J. A., and Linden, D. W. (2019). oSCR: a spatial capture–recapture R package for inference about spatial ecological processes. *Ecography*, 42(9):1459–1469.

2. R Core Team (2019) R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
