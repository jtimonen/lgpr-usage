#!/usr/bin/env Rscript

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
require(lgpr)

# Define a function that runs the experiment
runExperiment <- function(idx, n_aff){
  
  cat("This is experiment number ", idx, "\n", sep ="")

  if(n_aff==0){
    rel_da <- 0
  }else{
    rel_da <- 1
  }

  print(n_aff)

  # 1) Generate data
  simData <- simulate_data(N          = 16,
                         t_data       = seq(12, 72, by = 12),
                         covariates   = c(    0, 2,2,2),
                         relevances   = c(1,1,rel_da, 1,0,0),
                         lengthscales = c(18,24, 1, 18,18,18),
                         t_effect_range  = c(46,48),
                         snr          = 3,
                         N_affected   = n_aff,
                         t_jitter     = 0)

  # 2) Fit two models
  f1 <- lgp(formula      = y ~ id + age + diseaseAge + z1 + z2 + z3,
            data         = simData$data,
            equal_effect = TRUE,
            iter         = 2000,
            verbose      = TRUE)

  f2 <- lgp(formula      = y ~ id + age + diseaseAge + z1 + z2 + z3,
            data         = simData$data,
            equal_effect = FALSE,
            iter         = 2000,
            verbose      = TRUE)

  # 3) Select affected
  betas <- affected(f2, medians.return = TRUE)
  print(betas$median)

  # 4) Return results
  res  <- list(r1    = f1@relevances$average,
               r2    = f2@relevances$average,
               betas = betas,
               idx   = idx,
               f1    = f1,
               f2    = f2)
  return(res)
}

# Set number of affected individuals
NAFF    <- c(0,2,4,6,8)
idx_num <- as.numeric(idx_in)
n_aff   <- NAFF[ceiling(idx_num/100)]

# Run the experiment
res <- runExperiment(idx_in, n_aff)
fn  <- paste("res/res_naff_",n_aff,"_",idx_in,sep="")
save(res, file = fn)

