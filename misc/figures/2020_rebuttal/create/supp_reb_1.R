require(ggplot2)
require(ggpubr)

get_tpr_fpr <- function(r) {
  list(
    fpr = rev(1 - r$specificities),
    tpr = rev(r$sensitivities)
  )
}

# Helper function
create_roc_plot_df <- function(r1, r2) {
  v1 <- get_tpr_fpr(r1)
  v2 <- get_tpr_fpr(r2)
  auc1 <- round(r1$auc, digits = 3)
  auc2 <- round(r2$auc, digits = 3)
  method1 <- paste0("lgpr, auc = ", auc1)
  method2 <- paste0("lme4 + lmerTest, auc = ", auc2)
  
  TPR <- c(v1$tpr, v2$tpr)
  FPR <- c(v1$fpr, v2$fpr)
  L1 <- length(v1$tpr)
  L2 <- length(v2$tpr)
  cat("L1 =", L1, ", L2 =", L2, "\n") # show number of roc curve points
  met1 <- rep(method1, each = L1)
  met2 <- rep(method2, each = L2)
  MET <- c(met1, met2)
  df <- data.frame(as.factor(MET), FPR, TPR)
  colnames(df) <- c("Method", "FPR", "TPR")
  return(df)
}

# Helper function
create_roc_plot <- function(r1, r2) {
  df <- create_roc_plot_df(r1, r2)
  plt <- ggplot(df, aes(x = FPR, y = TPR, group = Method, color = Method)) +
    geom_line() + theme(legend.position = c(0.5,0.2))
  return(plt)
}

# Load results
res <- readRDS('res/res_noise.rds')
snr <- c(0.05, 0.1, 0.5, 1, 2, 3, 4, 5)
L <- length(snr)
plots <- list()
for (j in 1:L) {
  r <- res[[j]]$ROC
  plt <- create_roc_plot(r[[1]], r[[2]])
  plt <- plt + ggtitle(paste0('SNR = ', snr[j]))
  plots[[j]] <- plt
}

plt <- ggarrange(plotlist = plots, labels = "auto")
