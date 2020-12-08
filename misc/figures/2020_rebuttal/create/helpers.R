# Helper function
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
  auc1 <- round(r1$auc, digits = 4)
  auc2 <- round(r2$auc, digits = 4)
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
  aesth <- aes(x = FPR, y = TPR, group = Method, color = Method)
  plt <- ggplot(df, aesth) +
    geom_line() + theme(legend.position = c(0.5,0.2))
  return(plt)
}

# Helper function
create_rel_plot_df <- function(res, cnames) {
  rels <- res$LGPR_rel
  L <- nrow(rels)
  rels <- as.numeric(rels)
  comp <- rep(cnames, each = L)
  df <- data.frame(rels, comp)
  colnames(df) <- c("Relevance", "Component")
  return(df)
}

# Helper function
create_rel_plot <- function(res, cnames) {
  df <- create_rel_plot_df(res, cnames)
  aesth <- aes(y = Relevance, x = Component)
  plt <- ggplot(df, aesth) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(plt)
}

