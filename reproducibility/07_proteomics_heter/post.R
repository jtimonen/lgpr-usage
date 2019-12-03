#!/usr/bin/env Rscript
require(lgpr)

# Set paths
resDir    <- "res"
datDir    <- "../data/proteomics"
n_prot    <- 1538

# Load protein names
fn_Y   <- paste(datDir,"/liu_preproc_Y.csv",sep="")
Y_data <- read.csv(fn_Y, header=TRUE, sep=",")
pnames <- colnames(Y_data)[1:n_prot]

# Load inferred relevances and split R_hat statistics
REL     <- matrix(-1, n_prot, 6)
MRH     <- rep(-1, n_prot)
SEL     <- rep("foo", n_prot)
N_indiv <- rep(-1, n_prot)
n_data  <- rep(-1, n_prot)
N_cases <- rep(-1, n_prot)
FLAG    <- rep(0, n_prot) 
bad     <- c()
N_b02   <- rep(0, n_prot)
N_b05   <- rep(0, n_prot)

for(idx in 1:n_prot){
  fn <- paste(resDir, '/res_', idx, sep = "")
  cat('Loading file ', fn, '... ', sep="")
  tryCatch({
    load(fn) 
    cat('done!, max(Rhat) = ')
    fit       <- results$fit
    rel       <- fit@relevances$average
    mrh       <- max(fit@diagnostics$Rhat)
    selected  <- fit@selection$selected
    selected  <- selected[2:length(selected)] # first one is "noise"
    if('diseaseAge' %in% selected){
        cat(' *diseaseAge selected* ')
    }
    selected  <- paste(selected, collapse = ", ")
    REL[idx,] <- rel
    MRH[idx]  <- mrh
    SEL[idx]  <- selected
    n_data[idx]  <- fit@model@stan_dat$n
    N_indiv[idx] <- fit@model@stan_dat$N_tot
    N_cases[idx] <- fit@model@stan_dat$N_cases
    N_b02[idx] <- sum(affected(fit, threshold = 0.2))
    N_b05[idx] <- sum(affected(fit, threshold = 0.5))
    
    cat(paste(mrh))
    if(mrh > 1.05){
      cat(" BAD CONVERGENCE!")
      bad <- c(bad, idx)
    }
  }, error = function(e) {
    cat(" COULD NOT LOAD FILE!\n")
    FLAG[idx] <- 1
    warning(e)
  })

  cat("\n")
}

cat("The following runs converged badly: {")
cat(paste(bad, collapse = ","))
cat("} \n")

dnf <- which(FLAG==1)
cat("The following runs did not finish: {")
cat(paste(dnf, collapse = ","))
cat("} \n")

# Create result data frame
cnam <- c("idx", "name", "selected", names(rel), "max(Rhat)", "n", "N", "N_cases", "flag", "N_b05", "N_b02")
inds <- 1:n_prot
result_df <- data.frame(inds, pnames, SEL, REL, MRH, n_data, N_indiv, N_cases, FLAG, N_b05, N_b02, FLAG)
colnames(result_df) <- cnam

# Save results
saveRDS(result_df, file = "results.rds")

