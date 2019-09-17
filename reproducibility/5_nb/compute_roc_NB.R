# Path to the location where the results are
parentDir <- "../../../lgpr-results/exp_nb/res"
require(pROC)
require(ggplot2)

missing <- c()
idata <- setdiff(1:300, missing)
L     <- length(idata)
SCR   <- array(0, c(L,7,3))
rel   <- matrix(0, L, 6)
psig  <- rep(0, L)
j     <- 0
for(idx in idata){
  j  <- j + 1
  fn <- paste(parentDir, "/res_", idx, sep ="")
  load(fn) # loads a list called 'res'
  SCR[j,,1] <- res$r1$average
  SCR[j,,2] <- res$r2$average
  SCR[j,,3] <- res$r3$average
  psig[j]   <- res$simData$p_signal
  rel[j,]   <- res$rel_real
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
  
  Likelihood <- rep(c(paste("Gaussian, auc = ",    AUC[1], sep=""),
                 paste("NB, auc = ",  AUC[2], sep=""),
                 paste("log-Gaussian, auc = ",  AUC[3], sep="")),
               each = length(FPR)/3)
  
  DF <- data.frame(cbind(as.factor(Likelihood), FPR, TPR))
  
  h <- ggplot(DF, aes(x=FPR, y=TPR, colour=Likelihood, group = Likelihood)) +
    geom_line() + labs(x = "False positive rate", y = "True positive rate") +
    theme(legend.position = c(0.59,0.27), 
          legend.text=element_text(size=rel(0.78))) +
    scale_colour_brewer(type = "qual", palette = "Set1", direction = 1,
                        aesthetics = "colour")
  return(h + ggtitle(" "))
}

# Compute ROC curves
inds  <- c(1:6)
idat  <- 1:L
real  <- as.numeric(rel[idat,])
pred1 <- as.numeric(SCR[idat,inds,1])
pred2 <- as.numeric(SCR[idat,inds,2])
pred3 <- as.numeric(SCR[idat,inds,3])
roc1 <- roc(response = real, predictor = pred1)
roc2 <- roc(response = real, predictor = pred2)
roc3 <- roc(response = real, predictor = pred3)

plot <- createPlot(list(roc1,roc2,roc3))
