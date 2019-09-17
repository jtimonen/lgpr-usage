
require(pROC)
require(ggplot2)
require(ggpubr)

# Path to the location where the results are
parentDir <- "../../../lgpr-results/exp_uncrt/res"


# define some functions ---------------------------------------------------


# Loads computed relevances
loadREL <- function(parentDir){
  idata <- 1:300
  L     <- length(idata)
  REL   <- array(0, c(L,7,4))
  REAL  <- matrix(0, L, 6)
  
  for(idx in idata){
    fn <- paste(parentDir, "/res_", idx, sep ="")
    load(fn)
    REL[idx,,1] <- res$r0
    REL[idx,,2] <- res$r1
    REL[idx,,3] <- res$r2
    REL[idx,,4] <- res$r3
    REAL[idx,]  <- res$rel_real
  }
  return(list(REL=REL,REAL=REAL))
}


# Create an ROC plot
createPlot <- function(roc1, roc2, roc3, roc4){
  ROC <- list(roc1, roc2, roc3, roc4)
  FPR <- c( rev(1 - ROC[[1]]$specificities),
            rev(1 - ROC[[2]]$specificities),
            rev(1 - ROC[[3]]$specificities),
            rev(1 - ROC[[4]]$specificities))
  
  TPR <- c(rev(ROC[[1]]$sensitivities), 
           rev(ROC[[2]]$sensitivities),
           rev(ROC[[3]]$sensitivities),
           rev(ROC[[4]]$sensitivities))
  
  AUC <- round(c(ROC[[1]]$auc, 
                 ROC[[2]]$auc,
                 ROC[[3]]$auc,
                 ROC[[4]]$auc),
               digits = 3)
  
  method <- rep(c(paste("t_{onset} = 36, auc = ",    AUC[1], sep=""),
                  paste("t_{onset} ~ Uniform[0,36], auc = ",  AUC[2], sep=""),
                  paste("t_{diff} ~ Exponential(0.1), auc = ",  AUC[3], sep=""),
                  paste("t_{onset} ~ Log-Normal(3,0.2), auc = ",  AUC[4], sep="")),
                each = length(FPR)/4)
  
  DF <- data.frame(cbind(as.factor(method), FPR, TPR))
  
  h <- ggplot(DF, aes(x=FPR, y=TPR, colour=method, group = method)) +
    geom_line() + labs(x = "False positive rate", y = "True positive rate") +
    theme(legend.position = c(0.6,0.2), 
          legend.text=element_text(size=rel(0.78)), legend.title=element_blank()) +
    scale_colour_brewer(type = "qual", palette = "Set1", direction = 1,
                        aesthetics = "colour")
  return(h + ggtitle(" ") + theme(plot.title = element_text(size=11)))
}

# compute roc and create figures ------------------------------------------

# Load relevances
LIST <- loadREL(parentDir)
REL  <- LIST$REL
real <- LIST$REAL
idat <- c(1:300)
icov <- c(1:6)

# Compute AUROC
resp_A  <- as.numeric(real[idat,])
pred_A1 <- as.numeric(REL[idat,icov,1])
pred_A2 <- as.numeric(REL[idat,icov,2])
pred_A3 <- as.numeric(REL[idat,icov,3])
pred_A4 <- as.numeric(REL[idat,icov,4])
roc_A1  <- roc(response = resp_A, predictor = pred_A1)
roc_A2  <- roc(response = resp_A, predictor = pred_A2)
roc_A3  <- roc(response = resp_A, predictor = pred_A3)
roc_A4  <- roc(response = resp_A, predictor = pred_A4)

# Plot ROC curves
pA <- createPlot(roc_A1, roc_A2, roc_A3, roc_A4)

# Compute ROC for only the diseaseAge selection
pred_B1   <- REL[idat,3,1]
pred_B2   <- REL[idat,3,2]
pred_B3   <- REL[idat,3,3]
pred_B4   <- REL[idat,3,4]
resp_B    <- real[idat,3]
roc_B1    <- roc(response = resp_B, predictor = pred_B1)
roc_B2    <- roc(response = resp_B, predictor = pred_B2)
roc_B3    <- roc(response = resp_B, predictor = pred_B3)
roc_B4    <- roc(response = resp_B, predictor = pred_B4)
pB        <- createPlot(roc_B1, roc_B2, roc_B3, roc_B4) + labs(y = " ")#+ ggtitle('Disease covariate selection')


# Final ggplot
plot <- ggarrange(pA, pB, labels = c("a", "b"), ncol = 2)
