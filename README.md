# Using the lgpr package
This repository contains tutorials and experiments with the R package `lgpr` (https://github.com/jtimonen/lgpr). 

## Getting started
Tutorials in html format, package documentation and installation instructions can be found [here](https://jtimonen.github.io/lgpr-usage/).

## Reproducibility
The `reproducibility` directory contains codes for reproducing the experiments in the manuscript. Experiments involve fitting models for different data sets in parallel, and shell scripts for doing this on a computing cluster are provided. There are also scripts for collecting the final results and plotting ROC curves after the models have been fit for all data sets.

There are seven different experiments:

* `01_lme4` - comparison with linear mixed effect modeling and scalability testing
* `02_longp` - comparison with *LonGP* (https://www.nature.com/articles/s41467-019-09785-8)
* `03_heter` - heterogeneous disease effect modeling
* `04_effect_time` - modeling uncertainty of disease effect time
* `05_nb` - experiment with negative binomial distributed count data
* `06_proteomics` - proteomics data set modeling with a homogeneous disease effect
* `07_proteomics_heter` - proteomics data set modeling with a heterogeneous disease effect
