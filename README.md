# Simulation code
### Towards optimal sampling design for spatial capture-recapture


This repository contains the code to generate and evaluate SCR sampling designs from the framework presented the manudscript. The R code, found in the R subdirectory, operates within the file structure of this repository. Simulations are performed in the `6_sims.r` file, which pulls other files from the directory that are conveniently compiled into: `scr_design_sims.Rproj`. This file is currently parameterized to run all simulations simultaneously, and for safe of efficiency, all of the simulations are distributed across several cores.

Sampling designs evaluated here were generated within the R package [`oSCR`](https://sites.google.com/site/spatialcapturerecapture/oscr-package) [(Sutherland et al., 2019)](https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.04551) using the function `scrdesignGA`. You can download oSCR from GitHub using the following code:

```r
remotes::install_github("jaroyle/oSCR")
```

The help file for `scrdesignGA` can be accessed through the package, using: `?scrdesignGA`, and is also available [online](https://rdrr.io/github/jaroyle/oSCR/man/scrdesignGA.html).

Simulations were completed using R version 3.6.1.

1. Sutherland, C., Royle, J. A., and Linden, D. W. (2019). oSCR: a spatial capture–recapture R405package for inference about spatial ecological processes.Ecography, 42(9):1459–1469.
