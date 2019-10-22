#!/usr/bin/env Rscript
require(lgpr)
require(ggplot2)

# Set paths
parentDir <- "../.."
resDir    <- paste(parentDir, "/B_heter/res", sep="")
datDir    <- paste(parentDir, "/data_preproc", sep="")
n_prot    <- 1538

# Load protein names
fn_Y   <- paste(datDir,"/liu_preproc_Y.csv",sep="")
Y_data <- read.csv(fn_Y, header=TRUE, sep=",")
pnames <- colnames(Y_data)[1:n_prot]

# Plots
for(idx in 1:n_prot){
  fn <- paste(resDir, '/heter_', idx, sep = "")
  cat('Loading file ', fn, '... ', sep="")
  load(fn) 
  cat('done!\n')

  fit   <- results$fit
  data  <- results$data 
  
  p1    <- plot_components(fit, title = pnames[idx])

  t_test <- seq(0, 200, length.out = 60)
  X_test <- create_test_points(fit, t_test)
  PRED   <- lgp_predict(fit, X_test, samples = "mean")
  p2     <- plot_posterior_y(fit, PRED) + ggtitle(pnames[idx])
  p3     <- plot_posterior_f(fit, PRED, componentwise = T) + ggtitle(pnames[idx])
  p4     <- plot_beta(fit)

  fn1 <- paste("figs/comp_",  idx, "_", pnames[idx],".png",sep="")
  fn2 <- paste("figs/post_y_", idx, "_", pnames[idx],".png",sep="")
  fn3 <- paste("figs/post_f_", idx, "_", pnames[idx],".png",sep="")
  fn4 <- paste("figs/beta_", idx, "_", pnames[idx],".png",sep="")

  ggsave(filename = fn4, plot = p4, width = 8, height = 5.5, units = "in")
  ggsave(filename = fn1, plot = p1, width = 8, height = 5.5, units = "in")
  ggsave(filename = fn2, plot = p2, width = 8, height = 5.5, units = "in")
  ggsave(filename = fn3, plot = p3, width = 8, height = 8, units = "in")
  
}


