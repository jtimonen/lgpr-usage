#!/usr/bin/env Rscript

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]

require(lgpr)
require(ggplot2)

# Function for creating the data and input files for LonGP
data_to_LonGP <- function(data, idx){

  base <- paste(getwd(),"/longp/", sep="")
  dir.create(paste(base,"results/res",idx,sep=""))

  # Create the first 7 lines of input.para.txt
  line1 <- paste("inputX=", base, "data/X", idx, ".txt isCol=1", sep= "")
  line2 <- paste("inputY=", base, "data/y", idx, ".txt isCol=1", sep= "")
  line3a <- paste("resDir=", base, "results/res", idx, sep = "")
  line3b <- paste("resDir=", base, "results/res", idx, sep = "")  
  line4 <- "nConVar=6"
  line5 <- "nBinVar=5"
  line6 <- paste("priorFile=", base, "conf/prior.txt", sep="")
  line7 <- paste("kernelFile=", base, "conf/kernel.txt", sep="")

  # Create input.para.txt
  fn <-"longp/input_template.txt"

  # Create for scv 0.95
  text     <- readChar(fn, file.info(fn)$size)
  rows1to7  <- paste(paste(line1, line2, line3a, line4, line5, line6, line7,
                          sep="\n"), "\n", sep="")
  text     <- paste(rows1to7, text, sep="")
  text     <- gsub("\r", "", text)
  fileConn <- file(paste("longp/results/res",idx,"/input.para.txt", sep=""))
  writeLines(text, fileConn, sep="")
  close(fileConn)


  # Create the data files
  xname <- paste("longp/data/X", idx, ".txt", sep= "")
  yname <- paste("longp/data/y", idx, ".txt", sep= "")
  dat_X <- data[,c(2:11,1)]
  dat_y <- as.matrix(data$y)
  colnames(dat_y) <- "y"
  write.table(dat_X, na= "NaN", quote = F, file = xname, sep = "\t", row.names = F)
  write.table(dat_y, na= "NaN", quote = F, file = yname, sep = "\t", row.names = F)
}


# Function for running first part
run_part1 <- function(idx){
  
  if(as.numeric(idx) > 100){
    rel_da <- 0
  }else{
    rel_da <- 1
  }

  cat("idx = ", idx,  "\n", sep ="")
  print(rel_da)
  stp <- rnorm(n = 1, mean = 0.5, sd = 0.1)
  print(stp)

  # generate data
  simData <- simulate_data(N          = 16,
                         t_data       = seq(12, 72, by = 12),
                         covariates   = c(          0, 1,1,1,1, 2,2,2,2),
                         relevances   = c(1,1, rel_da, 1,1,0,0, 1,1,0,0),
                         lengthscales = c(12,24, 1,    1,1,1,1, 12,12,12,12),
                         t_effect_range = c(36,48),
                         t_jitter     = 1,
                         steepness    = stp)

  data <- simData$data
  p    <- plot_components_simdata(simData)
  fn   <- paste("figs/data_", idx, ".png", sep = "")
  ggsave(p, file = fn)
 
  # run lgpr
  pr <- prior_default()
  fit <- lgp(formula = y ~ id + age + diseaseAge + x1 + x2 + x3 + x4 + z1 + z2 + z3 + z4,
             data    = data,
             prior   = pr,
             chains  = 4,
             iter    = 2000)
  
  ret <- list(data  = data, 
              fit   = fit,
              idx   = idx)

  # Save data for longp
  data_to_LonGP(data, idx) 
  return(ret)
}

# run the comparison
res <- run_part1(idx_in)

# save results
saveRDS(res, file = paste("res/lgpr_",idx_in,".rds",sep=""))


