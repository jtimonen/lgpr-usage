#!/usr/bin/env Rscript
require(lgpr)

# Set paths
resDir    <- "res"
n_genes   <- 519

# Load inferred relevances and split R_hat statistics
REL     <- matrix(-1, n_genes, 6)
MRH     <- rep(-1, n_genes)
SEL     <- rep("foo", n_genes)
gnames  <- rep("gene", n_genes)
N_indiv <- rep(-1, n_genes)
n_data  <- rep(-1, n_genes)
N_cases <- rep(-1, n_genes)
FLAG    <- rep(0, n_genes) 
bad     <- c()

for(idx in 1:n_genes){
  fn <- paste(resDir, '/res_', idx, sep = "")
  cat('Loading file ', fn, '... ', sep="")
  tryCatch({
    load(fn) 
    cat('done!, max(Rhat) = ')
    gnames[idx] <- results$gene_name
    fit       <- results$fit
    rel       <- fit@relevances$average
    mrh       <- max(fit@diagnostics$Rhat)
    selected  <- fit@selection$selected
    selected  <- selected[2:length(selected)] # first one is "noise"
    selected  <- paste(selected, collapse = ", ")
    REL[idx,] <- rel
    MRH[idx]  <- mrh
    SEL[idx]  <- selected
    n_data[idx]  <- fit@model@stan_dat$n
    N_indiv[idx] <- fit@model@stan_dat$N_tot
    N_cases[idx] <- fit@model@stan_dat$N_cases
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
cnam <- c("idx", "name", "selected", names(rel), "max(Rhat)", "n", "N", "N_cases", "flag")
inds <- 1:n_genes
result_df <- data.frame(inds, gnames, SEL, REL, MRH, n_data, N_indiv, N_cases, FLAG)
colnames(result_df) <- cnam

# Save results
saveRDS(result_df, file = "results.rds")

