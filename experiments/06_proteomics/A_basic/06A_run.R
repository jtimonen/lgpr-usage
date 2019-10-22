#!/usr/bin/env Rscript

# This R script is meant to be called by a shell script with two integer arguments and one float

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
N_ITER <- as.numeric(args[2])
ADAPT_DELTA <- as.numeric(args[3])

require(lgpr)
  
 #Function for reading data
readLiuData <- function(parentDir, protein){
  fn_X   <- paste(parentDir,"/liu_preproc_X.csv",sep="")
  fn_Y   <- paste(parentDir,"/liu_preproc_Y.csv",sep="")
  X_data <- read.csv(fn_X, header=TRUE, sep=",")
  X_data <- X_data[,1:5]
  Y_data <- read.csv(fn_Y, header=TRUE, sep=",")
  names  <- colnames(Y_data)
  if(!is.character(protein)){
    pname <- names[protein]
  }else{
    pname <- protein
  }
  cat("Read data for protein '", pname, "'. \n", sep = "")
  y      <- Y_data[[pname]]
  notnan <- which(!is.nan(y))
  n_nan  <- length(which(is.nan(y)))
  data   <- data.frame(cbind(X_data, y))
  data   <- data[notnan, ]
  cat("Removed ", n_nan , " rows with NaN value for the response variable.\n", sep = "")
  return(data)
}

# Control parameters
CNTR  <- list(adapt_delta = ADAPT_DELTA)

# Read data
i_prot    <- as.numeric(idx_in)
parentDir <- "../data_preproc"
data      <- readLiuData(parentDir, i_prot)

# Set prior
my_prior <- prior_LonGP()
my_prior$lengthscale <- prior_default()$lengthscale

# Run lgp
dir.create('res')
fit <- lgp(formula       = y ~ id + age + diseaseAge + sex + group,
           data          = data,
           prior         = my_prior,
           offset_vars   = "group",
           control       = CNTR,
           iter          = N_ITER,
           save_warmup   = FALSE,
           variance_mask = TRUE)

# Return results
results <- list(data = data, fit = fit, n_iter = N_ITER, adapt_delta = ADAPT_DELTA)
save(results, file = paste("res/basic_", i_prot, sep=""))


