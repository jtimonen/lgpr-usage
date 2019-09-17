#!/usr/bin/env Rscript

# This R script is meant to be called by a shell script with an integer argument

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
require(lgpr)

# Define a function that runs the experiment
runExperiment <- function(idx){
  
  idx_num <- as.numeric(idx)
  if(idx_num <= 100){
     relev <- c(1,1,0,1,0,0)
  }else if(idx_num <= 200){
     relev <- c(0,1,1,1,0,0)
  }else{
     relev <- c(1,1,1,1,0,0)
  }
  print(relev)

  cat("This is experiment number ", idx, "\n", sep ="")
  onset_fun <- function(){rnorm(n = 1, mean = 36, sd = 4)}
  obs_fun   <- function(t){min(t + stats::rexp(n = 1, rate = 0.05), 95.99999)}

  # simulate data
  simData <- simulate_data(N           = 12,
                         t_data       = seq(12, 96, by = 12),
                         covariates   = c(    0,2,2,2),
                         relevances   = relev,
                         lengthscales = c(18,24, 1, 18,18,18),
                         onset_range  = onset_fun,
                         t_observed   = obs_fun,
                         snr          = 2)
  data <- simData$data
  N_ITER <- 1000
  
  # define priors and fit models -------------------------------------------
  p0 <- prior_default()
  f0 <- lgp(formula = y ~ id + age + diseaseAge + z1 + z2 + z3,
            data     = data,
            prior    = p0,
            iter     = N_ITER)

  p1       <- prior_default()
  p1$onset <- list(type="uniform_before")
  f1 <- lgp(formula   = y ~ id + age + diseaseAge + z1 + z2 + z3,
           data     = data,
           prior    = p1,
           iter     = N_ITER, 
           uncertain_diagnosis = TRUE)

  p2       <- prior_default()
  p2$onset <- list(type="gamma_before_backwards", shape = 1, rate = 0.05)
  f2 <- lgp(formula   = y ~ id + age + diseaseAge + z1 + z2 + z3,
          data     = data,
          prior    = p2,
          iter     = N_ITER, 
          uncertain_diagnosis = TRUE)

  p3       <- prior_default()
  p3$onset <- list(type="normal_whole", mu = 36, sigma = 4)
  f3 <- lgp(formula   = y ~ id + age + diseaseAge + z1 + z2 + z3,
          data     = data,
          prior    = p3,
          iter     = N_ITER, 
          uncertain_diagnosis = TRUE)

  # Return results
  res_small <- list(r0  = f0@covariate_relevances$average,
                    r1  = f1@covariate_relevances$average,
                    r2  = f2@covariate_relevances$average,
                    r3  = f3@covariate_relevances$average,
                    idx = idx,
                    rel_real = relev)
  return(res_small)
}


# Run the experiment
dir.create('res')
res <- runExperiment(idx_in)
save(res, file = paste("res/04_",idx_in,sep=""))



