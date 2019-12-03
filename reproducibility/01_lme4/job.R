#!/usr/bin/env Rscript

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
require(lgpr)

# Define a function that runs the experiment
runExperiment <- function(idx){
 
  idx_num <- as.numeric(idx)
  print(idx_num)
  relev   <- c(1,1,1,0,0)
  if(idx_num <= 100){
    N <- 20
    t_data <- seq(12, 60, length.out = 5)
  }else if(idx_num <= 200){
    N <- 30
    t_data <- seq(6, 60, length.out = 10)
  }else{
    N <- 30
    t_data <- seq(3, 60, length.out = 20)
  }
  print(N)
  print(t_data)

  simData <- simulate_data(N            = N,
                           t_data       = t_data,
                           covariates   = c(2,2,2),
                           relevances   = relev,
                           lengthscales = c(12,24,12,12,12),
                           t_jitter     = 0,
                           snr          = 0.2)
  data <- simData$data

  # lgpr
  fit_lgp <- lgp(y ~ id + age + z1 + z2 + z3, 
               data    = data,
               iter    = 1000, 
               chains  = 4,
               refresh = 500)
  rel <- fit_lgp@relevances$average

  # Return 
  res <- list(rel   = rel,
              real  = relev,
              fit   = fit_lgp,
              idx   = idx,
              simData = simData)
  return(res)
}

# Run the experiment
res <- runExperiment(idx_in)
fn  <- paste("res/res_", idx_in, ".rds", sep="")
saveRDS(res, file = fn)


