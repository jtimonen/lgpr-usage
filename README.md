# Demos and experiments using the lgpr package

## demos
The easiest way to study the demos is to open the *.html* files using any web browser or other software. The demos contain code blocks, which can be run in R if the `lgpr`package is installed. There are six different demos:
* `01_basic` - basic usage of the package
* `02_realdata` - modeling the classic orthodont and sleepstudy datasets
* `03_comparison` - comparison with linear mixed effect modeling
* `04_disease` - modeling nonstationary disease effects with different modeling options
* `05_nongaussian` - modeling discrete count data using a non-Gaussian observation model
* `06_predicting` - computing out-of-sample predictions

## experiments
Contains codes for reproducing the experiments in the manuscript. Each subfolder has an experiment info which describes how to repeat the experiment. Experiments involve fitting models for different data sets in parallel, and shell scripts for doing this on a computing cluster are provided. There are six different experiments:

* `01_lme4` - comparison with linear mixed effect modeling and scalability testing
* `02_longp` - comparison with *LonGP* (https://www.nature.com/articles/s41467-019-09785-8)
* `03_heter` - heterogeneous disease effect modeling
* `04_uncrt` - modeling uncertainty of disease onset
* `05_nb` - experiment with negative binomial distributed count data
* `06_proteomics_data` - real proteomics data set experiments
