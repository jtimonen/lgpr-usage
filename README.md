# Using the lgpr package
This repository contains tutorials and experiments with the R package **lgpr** (https://github.com/jtimonen/lgpr). 

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
* `06_proteomics` - longitudinal proteomics data set [1] modeling with a homogeneous disease effect
* `07_proteomics_heter` - longitudinal proteomics data set [1] modeling with a heterogeneous disease effect
* `08_rna-seq` - analysis of longitudinal RNA-seq data from CD4+ cells [2]

## References 

*[1]* Liu *et al.* Temporal expression profiling of plasma proteins reveals oxidative stress in early stages of Type 1 Diabetes progression (2018). *Journal of proteomics* 172: 100-110. doi:10.1016/j.jprot.2017.10.004

*[2]* Kallionpää *et al.* Early Detection of Peripheral Blood Cell Signature in Children Developing β-Cell Autoimmunity at a Young Age (2019). *Diabetes*  68(10):2024-2034. doi:10.2337/db19-0287
