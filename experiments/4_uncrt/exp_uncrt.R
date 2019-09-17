#!/usr/bin/env Rscript

# This R script is meant to be called by a shell script with an integer argument
# For example:
#   n=$SLURM_ARRAY_TASK_ID
#   srun Rscript --vanilla exp_uncrt.R $n

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
  fun1 <- function(x){-1/2 + 1/(1 + exp(-2*x))} # sigmoidal rise

  onset_fun <- function(){stats::rnorm(n = 1, mean = 20, sd = 4)}
  obs_fun <- function(t){min(t + stats::rgamma(n = 1, shape = 3, rate = 0.5), 47.9)}

  # create data -------------------------------------------------------------
  simData <- simulate_data(N          = 12,
                         t_data       = c(6, 12, 18, 24, 30, 36, 42, 48),
                         #t_data       = seq(0,40,by=4),
                         covariates   = c(    0,2,2,2),
                         relevances   = relev,
                         lengthscales = c(6,6,1,6,6,6),
                         onset_range  = onset_fun,
                         t_observed   = obs_fun,
                         snr          = 1,
                         dis_fun      = fun1)

  data <- simData$data
  N_ITER <- 1000
  
  # define priors and fit models -------------------------------------------
  p0 <- prior_default()
  p0$warp_steepness <- list(type="log-normal", mu = 1, sigma = 0.2)
  f0 <- lgp(formula = y ~ id + age + diseaseAge + z1 + z2 + z3,
            data     = data,
            prior    = p0,
            iter     = N_ITER)

  p1       <- prior_default()
  p1$onset <- list(type="uniform_whole")
  p1$warp_steepness <- p0$warp_steepness
  f1 <- lgp(formula   = y ~ id + age + diseaseAge + z1 + z2 + z3,
           data     = data,
           prior    = p1,
           iter     = N_ITER, 
           uncertain_diagnosis = TRUE)

  p2       <- prior_default()
  p2$onset <- list(type="gamma_before_backwards", shape = 1, rate = 0.1)
  p2$warp_steepness <- p0$warp_steepness
  f2 <- lgp(formula   = y ~ id + age + diseaseAge + z1 + z2 + z3,
          data     = data,
          prior    = p2,
          iter     = N_ITER, 
          uncertain_diagnosis = TRUE)

  p3       <- prior_default()
  p3$onset <- list(type="normal_whole", mu = 20, sigma = 4)
  p3$warp_steepness <- p0$warp_steepness
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
res <- runExperiment(idx_in)
save(res, file = paste("res/res_",idx_in,sep=""))



