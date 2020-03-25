# Simulation code
### Towards optimal sampling design for spatial capture-recapture


This repository contains the code to generate and evaluate SCR sampling designs from the framework presented the manudscript. The R code, found in the R subdirectory, operates within the file structure of this repository. Simulations are performed in the `6_sims.r` file, which pulls other files from the directory that are conveniently compiled into: `scr_design_sims.Rproj`. This file is currently parameterized to run all simulations simultaneously, and for safe of efficiency, all of the simulations are distributed across several cores.


Simulations were completed using R version 3.6.1.
