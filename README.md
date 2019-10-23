
## Tutorials, installation and documentation
See [this page](https://jtimonen.github.io/lgpr-usage/)

## Experiments
The `experiments` directory contains codes for reproducing the experiments in the manuscript. Experiments involve fitting models for different data sets in parallel, and shell scripts for doing this on a computing cluster are provided. There are also scripts for collecting the final results and plotting ROC curves or other result figures, after the models have been fit for all data sets. There are six different experiments:

* `01_lme4` - comparison with linear mixed effect modeling and scalability testing
* `02_longp` - comparison with *LonGP* (https://www.nature.com/articles/s41467-019-09785-8)
* `03_heter` - heterogeneous disease effect modeling
* `04_uncrt` - modeling uncertainty of disease onset
* `05_nb` - experiment with negative binomial distributed count data
* `06_proteomics_data` - real proteomics data set experiments
