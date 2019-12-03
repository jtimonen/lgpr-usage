#!/usr/bin/env Rscript
args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
N_ITER <- as.numeric(args[2])
ADAPT_DELTA <- as.numeric(args[3])

require(lgpr)
require(ggplot2)
 
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
i_prot   <- as.numeric(idx_in)
dataDir  <- "../data/proteomics"
data     <- readLiuData(dataDir, i_prot)
figDir   <- paste0('figs/figs_', i_prot)
dir.create(figDir)

# Set prior
my_prior <- prior_default()

# Run lgp
fit <- lgp(formula       = y ~ id + age + diseaseAge + sex + group,
           data          = data,
           prior         = my_prior,
           offset_vars   = "group",
           control       = CNTR,
           iter          = N_ITER,
           save_warmup   = FALSE,
           variance_mask = TRUE,
           verbose       = TRUE)

print(fit@diagnostics)

# Return results
results <- list(data = data, fit = fit, n_iter = N_ITER, adapt_delta = ADAPT_DELTA)
save(results, file = paste("res/res_", i_prot, sep=""))

# Plots
FONT_SIZE <- 14
p1     <- plot_components_posterior(fit, font_size = FONT_SIZE)
t_test <- seq(0, 200, length.out = 100)
X_test <- create_test_points(fit, t_test)
PRED   <- lgp_predict(fit, X_test, samples = "mean", print_params = TRUE)
p2     <- plot_posterior_y(fit, PRED)
p3     <- plot_components_posterior(fit, PRED = PRED, font_size = FONT_SIZE)
#p4    <- plot_beta(fit)

fn1 <- paste(figDir, "/c.png", sep="")
fn2 <- paste(figDir, "/y.png", sep="")
fn3 <- paste(figDir, "/f.png", sep="")
#fn4 <- paste(figDir, "/b.png", sep="")

#ggsave(filename = fn4, plot = p4, width = 8, height = 5.5, units = "in")
ggsave(filename = fn1, plot = p1, width = 8, height = 5.5,  units = "in")
ggsave(filename = fn2, plot = p2, width = 8, height = 5.5,  units = "in")
ggsave(filename = fn3, plot = p3, width = 8, height = 8, units = "in")


