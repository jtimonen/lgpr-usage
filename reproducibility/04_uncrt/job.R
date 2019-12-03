#!/usr/bin/env Rscript

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
  init_fun <- function(){rnorm(n = 1, mean = 36, sd = 4)}
  obs_fun   <- function(t){min(t + stats::rexp(n = 1, rate = 0.05), 95.99999)}

  # simulate data
  simData <- simulate_data(N           = 12,
                         t_data       = seq(12, 96, by = 12),
                         covariates   = c(    0,2,2,2),
                         relevances   = relev,
                         lengthscales = c(18,24, 1, 18,18,18),
                         t_effect_range  = init_fun,
                         t_observed   = obs_fun,
                         snr          = 1)
  data <- simData$data
  N_ITER <- 2000
  
  # define priors and fit models -------------------------------------------
  p1 <- prior_default()
  f1 <- lgp(formula = y ~ id + age + diseaseAge + z1 + z2 + z3,
            data     = data,
            prior    = p1,
            iter     = N_ITER,
            verbose  = TRUE,
            refresh  = 0)

  p2          <- prior_default()
  p2$t_effect <- list(type="gamma_before_backwards", shape = 1, rate = 0.05)
  f2 <- lgp(formula   = y ~ id + age + diseaseAge + z1 + z2 + z3,
          data     = data,
          prior    = p2,
          iter     = N_ITER, 
          uncertain_effect_time = TRUE,
          verbose = TRUE,
          refresh = 0)

  p3          <- prior_default()
  p3$t_effect <- list(type="normal_whole", mu = 36, sigma = 4)
  f3 <- lgp(formula   = y ~ id + age + diseaseAge + z1 + z2 + z3,
          data     = data,
          prior    = p3,
          iter     = N_ITER, 
          uncertain_effect_time = TRUE,
          verbose = TRUE,
          refresh = 0)

  # Return results
  res_large <- list(r1  = f1@relevances$average,
                    r2  = f2@relevances$average,
                    r3  = f3@relevances$average,
                    f1  = f1,
                    f2  = f2,
                    f3  = f3,
                    idx = idx,
                    rel_real = relev)
  return(res_large)
}


# Run the experiment
res <- runExperiment(idx_in)
save(res, file = paste("res/res_",idx_in,sep=""))



