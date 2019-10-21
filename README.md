# Demos and experiments using the R-package 'lgpr'

## demos
The demos are in *.html* format, and after downloading you can open them using any web browser or other software. To avoid downloading, you can view the demos also at https://users.aalto.fi/~timonej3/2-lgpr.html. The demos contain code blocks, which can be run in R if the `lgpr` package is installed.
* `01_basic` - basic usage of the package
* `02_sleepstudy_and_orthodont` - modeling the classic orthodont and sleepstudy datasets
* `03_comparison` - comparison with linear mixed effect modeling
* `04_disease` - modeling nonstationary disease effects with different modeling options
* `05_nongaussian` - modeling discrete count data using a non-Gaussian observation model
* `06_predicting` - computing out-of-sample predictions

## experiments
Contains codes for reproducing the experiments in the manuscript. Experiments involve fitting models for different data sets in parallel, and shell scripts for doing this on a computing cluster are provided. There are also scripts for collecting the final results and plotting ROC curves or other result figures, after the models have been fit for all data sets. There are six different experiments:

* `01_lme4` - comparison with linear mixed effect modeling and scalability testing
* `02_longp` - comparison with *LonGP* (https://www.nature.com/articles/s41467-019-09785-8)
* `03_heter` - heterogeneous disease effect modeling
* `04_uncrt` - modeling uncertainty of disease onset
* `05_nb` - experiment with negative binomial distributed count data
* `06_proteomics_data` - real proteomics data set experiments
