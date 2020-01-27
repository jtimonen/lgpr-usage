require(pROC)

# Create a data frame for a ROC plot
create_roc_df <- function(real, PRED, digits_auc = 3){
  nam   <- colnames(PRED)
  n     <- dim(PRED)[1]
  n_met <- dim(PRED)[2]
  FPR   <- c()
  TPR   <- c()
  MET   <- c()
  for(j in 1:n_met){
    pred <- as.numeric(PRED[,j])
    roc  <- roc(response = real, predictor = pred)
    fpr  <- rev(1 - roc$specificities)
    tpr  <- rev(roc$sensitivities)
    leg  <- paste0(nam[j], ', auc = ', round(roc$auc, digits_auc))
    met  <- rep(leg, length(tpr))
    FPR  <- c(FPR, fpr)
    TPR  <- c(TPR, tpr)
    MET  <- c(MET, met)
  }

  method <- as.factor(MET)
  DF <- data.frame(method, FPR, TPR)
  return(DF)
}

# Create an ROC plot
create_roc_plot <- function(df){
  h <- ggplot(df, aes(x=FPR, y=TPR, colour=method, group = method)) +
    geom_line() + labs(x = "False positive rate", y = "True positive rate") +
    theme_bw()
  h <- h + theme(panel.grid.minor = element_blank()) + 
    theme(legend.title=element_blank(), 
          legend.text=element_text(size=7))
  h <- h + theme(legend.position = c(0.5, 0.3))
  
  return(h)
}

# Wrap both
create_roc <- function(real, PRED, digits_auc = 3){
  df    <- create_roc_df(real, PRED, digits_auc)
  plt   <- create_roc_plot(df)
  return(plt)
}


# Create ROC for lme4 comparison
roc_lme4 <- function(res, palette){
  score1 <- res$lgpr_a
  score2 <- res$lmer
  PRED   <- data.frame(score1, score2)
  colnames(PRED) <- c("lgpr", "lme4 + lmerTest")
  p      <- create_roc(res$resp, PRED)
  p      <- p + scale_color_brewer(type = "qual", palette = palette)
  return(p)
}

# Create ROC for likelihoods comparison
roc_nb <- function(res, palette){
  score1 <- res$rel$pred1
  score2 <- res$rel$pred2
  score3 <- res$rel$pred3
  PRED   <- data.frame(score1, score2, score3)
  colnames(PRED) <- c("Gaussian", "NB", "log-Gaussian")
  p      <- create_roc(res$rel$real, PRED)
  p      <- p + scale_color_brewer(type = "qual", 
                                   palette = palette,
                                   direction = -1)
  return(p)
}

# Create ROC for effect time
roc_et <- function(res, palette){
  real_a  <- res[[1]]$original.response
  score1a <- res[[1]]$original.predictor
  score2a <- res[[2]]$original.predictor
  score3a <- res[[3]]$original.predictor
  
  real_b  <- res[[4]]$original.response
  score1b <- res[[4]]$original.predictor
  score2b <- res[[5]]$original.predictor
  score3b <- res[[6]]$original.predictor
  
  PRED1   <- data.frame(score1a, score2a, score3a)
  PRED2   <- data.frame(score1b, score2b, score3b)
  colnames(PRED1) <- c("Fixed", "Uncertain1", "Uncertain2")
  colnames(PRED2) <- c("Fixed", "Uncertain1", "Uncertain2")
  
  pa      <- create_roc(real_a, PRED1)
  pa      <- pa + scale_color_brewer(type = "qual", 
                                   palette = palette,
                                   direction = -1)
  
  pb      <- create_roc(real_b, PRED2)
  pb      <- pb + scale_color_brewer(type = "qual", 
                                     palette = palette,
                                     direction = -1)
  
  return(list(a = pa, b = pb))
}


# Violin plot of beta
create_boxplot <- function(BETA, cs = "red"){
  N <- dim(BETA)[1]
  D <- dim(BETA)[2]
  id <- rep(1:D, each=N)
  median <- as.numeric(BETA)
  df <- data.frame(cbind(as.factor(id), median))
  colnames(df) <- c("id", "Median")
  fill <- bayesplot::color_scheme_get(cs)$mid
  col  <- bayesplot::color_scheme_get(cs)$dark
  h <- ggplot(df, aes(x=id, y=Median, group=id)) + 
    geom_boxplot(width = 0.45, 
                 col = col, 
                 fill = fill,
                 outlier.size = 0,#1, 
                 outlier.stroke = 0)#0.5) 
  #h <- h + theme_bw()
  h <- h + ylab(expression(beta[id]))
  return(h)
}


# Create ROC for heterogeneous modeling
roc_heter_one <- function(res, rel_da, palette){
  real   <- c(1,1,rel_da, 1,0,0)
  real   <- rep(real, each=100)
  inds   <- c(1:6)
  score1 <- as.numeric(res$REL[,inds,1])
  score2 <- as.numeric(res$REL[,inds,2])
  PRED   <- data.frame(score1, score2)
  colnames(PRED) <- c("Homogeneous", "Heterogeneous")
  gg_beta <- create_boxplot(res$BET)
  
  p <- create_roc(real, PRED)
  p <- p + scale_color_brewer(type = "qual", 
                                     palette = palette,
                                     direction = 1)
  
  return(list(roc = p, beta = gg_beta))
}



