# Path to the location where the results are
parentDir <- "../../../lgpr-results/exp_lme4/res"
require(pROC)
require(ggplot2)
require(ggpubr)

idata <- 1:300
L     <- length(idata)
REL1  <- matrix(0, L, 4)
REL2  <- matrix(0, L, 5)
REAL  <- matrix(0, L, 4)
for(idx in idata){
  fn <- paste(parentDir, "/res_", idx, sep ="")
  load(fn)
  REL1[idx,] <- res$fval
  REL2[idx,] <- res$lgpr
  REAL[idx,] <- res$real
}

# Compute ROC
inds  <- c(3:4) # just sex and location
resp  <- as.numeric(REAL[,inds])
pred1 <- as.numeric(REL1[,inds])
pred2 <- as.numeric(REL2[,inds])
roc1  <- roc(response = resp, predictor = pred1)
roc2  <- roc(response = resp, predictor = pred2)

# A For plotting
createPlot <- function(ROC){
  FPR <- c( rev(1 - ROC[[1]]$specificities),
            rev(1 - ROC[[2]]$specificities))
  
  TPR <- c(rev(ROC[[1]]$sensitivities), 
           rev(ROC[[2]]$sensitivities))
  
  AUC <- round(c(ROC[[1]]$auc, 
                 ROC[[2]]$auc),
               digits = 3)
  
  method <- rep(c(paste("lme4, auc = ",  AUC[1], sep=""),
                  paste("lgpr, auc = ",  AUC[2], sep="")),
                each = length(FPR)/2)
  
  DF <- data.frame(cbind(as.factor(method), FPR, TPR))
  
  h <- ggplot(DF, aes(x=FPR, y=TPR, colour=method, group = method)) +
    geom_line() + labs(x = "False positive rate", y = "True positive rate") +
    theme(legend.position = c(0.65,0.2), 
          legend.text=element_text(size=rel(0.78)), legend.title=element_blank()) +
    scale_colour_brewer(type = "qual", palette = "Set1", direction = 1,
                        aesthetics = "colour")
  return(h + ggtitle(" "))
}

# Run the result handling and create the ROC plots
h1 <- createPlot(list(roc1,roc2))
