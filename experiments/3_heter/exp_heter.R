#!/usr/bin/env Rscript

# This R script is meant to be called by a shell script with an integer argument
# For example:
#   n=$SLURM_ARRAY_TASK_ID
#   srun Rscript --vanilla exp_heter.R $n

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
require(lgpr)

# Define a function that runs the experiment
runExperiment <- function(idx, n_aff){
  
  cat("This is experiment number ", idx, "\n", sep ="")

  # 1) Generate data
  simData <- simulate_data(N          = 16,
                        t_data       = c(6,12,18,24,30,36),
                        covariates   = c(    0, 2,2,2),
                        relevances   = c(1,1,1, 1,0,0),
                        lengthscales = c(6,6,1, 6,6,6),
                        onset_range  = c(12, 24),
                        snr          = 5,
                        N_affected   = n_aff,
                        t_jitter     = 1)
 
  # 2) Fit two models
  f1 <- lgp(formula = y ~ id + age + diseaseAge + z1 + z2 + z3,
                   data = simData$data,
                   equal_effect = TRUE,
                   iter = 1000)

  f2 <- lgp(formula = y ~ id + age + diseaseAge + z1 + z2 + z3,
                   data = simData$data,
                   equal_effect = FALSE,
                   iter = 1000)

  # 3) Select affected
  betas <- affected(f2, medians.return = TRUE)
  print(betas$median)

  # 4) Return results
  res_small <- list(r1    = f1@covariate_relevances$average,
                    r2    = f2@covariate_relevances$average,
                    betas = betas,
                    idx   = idx)
  return(res_small)
}

# Run the experiments
res2 <- runExperiment(idx_in, 2)
res4 <- runExperiment(idx_in, 4)
res6 <- runExperiment(idx_in, 6)
res8 <- runExperiment(idx_in, 8)
res <- list(res2=res2, res4=res4, res6=res6, res8=res8)
save(res, file = paste("res/res_",idx_in,sep=""))


