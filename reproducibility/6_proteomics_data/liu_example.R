
require(lgpr)

# Set protein idx (1-1538) here
i_prot <- 1343

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

# Sampling adaptation parameter
CNTR  <- list(adapt_delta = 0.99)

# Read data
parentDir <- "./data_preproc"
data      <- readLiuData(parentDir, i_prot)
plot_data(data)

# Set prior
my_prior <- prior_LonGP()
my_prior$lengthscale <- prior_default()$lengthscale
my_prior$warp_steepness <- list(type = "inv-gamma", shape = 9, scale = 3)
my_prior$onset <- list(type = "normal_relative", mu = 0, sigma = 20)

# Run lgp
fit <- lgp(formula      = y ~ id + age + diseaseAge + sex + group,
           data         = data,
           prior        = my_prior,
           offset_vars  = "group",
           control      = CNTR,
           iter         = 2000,
           uncertain_diagnosis = TRUE,
           equal_effect = FALSE,
           chains       = 1,
           save_warmup  = FALSE,
           refresh      = 50)

