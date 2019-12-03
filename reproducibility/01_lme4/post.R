#!/usr/bin/env Rscript

# This R script is meant to be called by a shell script
require(lgpr)
require(pROC)
require(ggplot2)
require(ggpubr)
require(lmerTest)

print(sessionInfo())
cat('\n')

compute_roc <- function(idata){
  L     <- length(idata)
  LGPR  <- matrix(0, L, 3)
  PV1   <- matrix(0, L, 3)
  REAL  <- matrix(0, L, 3)
  PROB  <- matrix(0, L, 3)

  j     <- 0 
  for(idx in idata){
    j <- j + 1

    # Read the lgpr results and real relevances
    resDir <- paste("res", sep ="")
    fn     <- paste(resDir, "/res_", idx, ".rds", sep ="")
    res    <- readRDS(fn)
    cat("Read file", fn, "\n")
    REAL[j,] <- res$real[3:5]
    fit      <- postproc(res$fit, threshold = 0.95)
    score_a  <- as.numeric(fit@relevances$average)
    score_b  <- as.numeric(selection(fit, threshold = 0.95)$prob)
    #print(score_a)
    #print(score_b)
    
    #print(prob)
    LGPR[j,] <- score_a[3:5]
    PROB[j,] <- score_b[3:5]

    # Run lme4
    data     <- fit@model@data
    fit1     <- lmerTest::lmer(y ~ 1 + age + (age|id) + z1 + z2 + z3, data = data)
    pv1      <- as.data.frame(drop1(fit1))$`Pr(>F)`
    PV1[j,]  <- as.numeric(pv1[2:4])
  }

  # Compute ROC
  resp   <- as.numeric(REAL)
  lgpr_a <- as.numeric(LGPR)
  lmer1  <- as.numeric(PV1)
  lgpr_b <- as.numeric(PROB)
  roc1   <- roc(response = resp, predictor = lgpr_a)
  roc2   <- roc(response = resp, predictor = lmer1)
  roc3   <- roc(response = resp, predictor = lgpr_b)

  ROC <- list(roc1, roc2, roc3)
  ret <- list(resp = resp, lgpr_a = lgpr_a, lgpr_b = lgpr_b, lmer=lmer1, ROC = ROC)
  return(ret)
}

# A For plotting
createPlot <- function(ROC){
  FPR <- c( rev(1 - ROC[[1]]$specificities),
            rev(1 - ROC[[2]]$specificities),
            rev(1 - ROC[[3]]$specificities))
  
  TPR <- c(rev(ROC[[1]]$sensitivities), 
           rev(ROC[[2]]$sensitivities),
           rev(ROC[[3]]$sensitivities))
  
  AUC <- round(c(ROC[[1]]$auc, 
                 ROC[[2]]$auc,
                 ROC[[3]]$auc),
               digits = 3)
  
  method <- rep(c(paste("lgpr-A, auc = ",  AUC[1], sep=""),
                  paste("lmer, auc = ",    AUC[2], sep=""),
                  paste("lgpr-B, auc = ",  AUC[3], sep="")),
                each = length(FPR)/3)
 
  method <- as.factor(method) 
  DF <- data.frame(method, FPR, TPR)
  print(head(DF))
 
  h <- ggplot(DF, aes(x=FPR, y=TPR, colour=method, group = method)) +
    geom_line() + labs(x = "False positive rate", y = "True positive rate") +
    theme(legend.position = c(0.65,0.2), 
          legend.text=element_text(size=rel(0.78)), legend.title=element_blank()) +
    scale_colour_brewer(type = "qual", palette = "Set1", direction = 1,
                        aesthetics = "colour")
  return(h + ggtitle(" "))
}

# Run the result handling and create the ROC plots
r100 <- compute_roc(1:100)
r300 <- compute_roc(101:200)
r600 <- compute_roc(201:300)

saveRDS(r100, file = 'roc_100.rds')
saveRDS(r300, file = 'roc_300.rds')
saveRDS(r600, file = 'roc_600.rds')
print(r100)
print(r300)
print(r600)

h1   <- createPlot(r100$ROC)
h2   <- createPlot(r300$ROC)
h3   <- createPlot(r600$ROC)

plot <- ggarrange(h1, h2, h3, labels = "auto", nrow = 1)
ggsave(filename =  "roc.png", plot = plot, width = 12, height = 4)
ggsave(filename = "roc.eps", plot = plot, width = 12, height = 4)


