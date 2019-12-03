#!/usr/bin/env Rscript

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
require(lgpr)

# Define a function that runs the experiment
runExperiment <- function(idx){
  
  cat("This is experiment number ", idx, "\n", sep ="")
  idx_num <- as.numeric(idx)
  if(idx_num <= 100){
    rel_real <- c(0,1,1,0,1,0)
  }else if(idx_num > 100 && idx_num <= 200){
    rel_real <- c(1,1,1,0,1,0)
  }else{
    rel_real <- c(0,0,1,1,0,0)
  }

  print(rel_real)
  simData <- simulate_data(N          = 10,
                         t_data       = seq(6, 48, by = 6),
                         covariates   = c(     1,1,2,2),
                         lengthscales = c(6,12,1,1,6,6),
                         relevances   = rel_real,
                         noise_type   = "NB",
                         phi          = 2,
                         C_hat        = -1,
                         f_var        = 2,
                         t_jitter     = 0.2)

  p_signal <- simData$p_signal
  print(p_signal)
  print(simData$data$y)
  N_ITER <- 4000
  CONTROL <- list(adapt_delta = 0.95)

  # gaussian model ----------------------------------------------------------

  f1 <- lgp(formula  = y ~ id + age + x1 + x2 + z1 + z2, 
          data     = simData$data,
          iter     = N_ITER, 
          parallel = FALSE, 
          chains   = 4,
          control  = CONTROL,
          save_warmup = FALSE)


  # nb model ----------------------------------------------------------------

  f2 <- lgp(formula    = y ~ id + age +  x1 + x2 +  z1 + z2,
          data       = simData$data,
          likelihood = "NB",
          iter       = N_ITER, 
          parallel   = FALSE, 
          chains     = 4,
          control    = CONTROL,
          save_warmup = FALSE)


  # log-gaussian model ------------------------------------------------------

  logdata    <- simData$data
  logdata$y  <- log(1 + logdata$y)

  f3 <- lgp(formula  = y ~ id + age + x1 + x2 + z1 + z2,
          data     = logdata,
          iter     = N_ITER, 
          parallel = FALSE, 
          chains   = 4,
          control  = CONTROL,
          save_warmup = FALSE) 

  # Show results
  cat("\n\n")
  show(f1)
  show(f2)
  show(f3)

  # Return results
  res_large <- list(r1 = f1@relevances$average,
                    r2 = f2@relevances$average,
                    r3 = f3@relevances$average,
                    f1 = f1,
                    f2 = f2,
                    f3 = f3,
                    simData = simData,
                    idx = idx,
                    rel_real = rel_real)
                    
  return(res_large)
}


# Run the experiment
res <- runExperiment(idx_in)
save(res, file = paste("res/res_",idx_in,sep=""))


