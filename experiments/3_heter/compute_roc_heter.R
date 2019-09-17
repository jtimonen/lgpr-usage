# Path to the location where the results are
parentDir <- "../../../lgpr-results/exp_heter/res"
require(pROC)
require(ggplot2)
require(ggpubr)

# A result loader function
get_res <- function(parentDir, n_aff){
  
  idata <- 1:100
  L     <- length(idata)
  REL   <- array(0, c(L,7,2))
  BET   <- matrix(0, L, 8)
  for(idx in idata){
      fn <- paste(parentDir, "/res_", idx, sep ="")
      load(fn)
      field <- paste("res",n_aff,sep="")
      resu <- res[[field]]
      REL[idx,,1] <- resu$r1
      REL[idx,,2] <- resu$r2
      BET[idx,]   <- resu$beta$medians
  }
  return(list(REL=REL,BET=BET))
}

# Compute ROC
compute_ROC <- function(res){
  real  <- c(1,1,1, 1,0,0)
  inds  <- c(1:6)
  resp  <- rep(real, each=100)
  pred1 <- as.numeric(res$REL[,inds,1])
  pred2 <- as.numeric(res$REL[,inds,2])
  roc1  <- roc(response = resp, predictor = pred1)
  roc2  <- roc(response = resp, predictor = pred2)
  return(list(roc1,roc2))
}


# A For plotting
createPlot <- function(ROC){
  FPR <- c( rev(1 - ROC[[1]]$specificities),
            rev(1 - ROC[[2]]$specificities))
  
  TPR <- c(rev(ROC[[1]]$sensitivities), 
           rev(ROC[[2]]$sensitivities))
  
  AUC <- round(c(ROC[[1]]$auc, 
                 ROC[[2]]$auc),
               digits = 3)
  
  Model <- rep(c(paste("homog., auc = ",    AUC[1], sep=""),
                  paste("heterog., auc = ",  AUC[2], sep="")),
                each = length(FPR)/2)
  
  DF <- data.frame(cbind(as.factor(Model), FPR, TPR))
  
  h <- ggplot(DF, aes(x=FPR, y=TPR, colour=Model, group = Model)) +
    geom_line() + labs(x = "False positive rate", y = "True positive rate") +
    theme(legend.position = c(0.55,0.27), 
          legend.text=element_text(size=rel(0.78))) +
    scale_colour_brewer(type = "qual", palette = "Set1", direction = 1,
                          aesthetics = "colour")
  return(h + ggtitle(" "))
}

# Violin plot of beta
create_violin <- function(BETA){
  N <- dim(BETA)[1]
  D <- dim(BETA)[2]
  id <- rep(1:D, each=N)
  median <- as.numeric(BETA)
  df <- data.frame(cbind(as.factor(id), median))
  colnames(df) <- c("id", "Median")
  h <- ggplot(df, aes(x=id, y=Median, group=id)) + geom_boxplot(width = 0.45, col = "gray40", fill = "gray70", outlier.size = 0, outlier.stroke = 0) 
  return(h)
}

# Get results
r2 <- get_res(parentDir, 2)
r4 <- get_res(parentDir, 4)
r6 <- get_res(parentDir, 6)
r8 <- get_res(parentDir, 8)

# Compute ROC
R2 <- compute_ROC(r2)
R4 <- compute_ROC(r4)
R6 <- compute_ROC(r6)
#R8 <- compute_ROC(r8)

# Plot ROC
h2 <- createPlot(R2) + ggtitle("2 affected")
h4 <- createPlot(R4) + ggtitle("4 affected") + labs(y = " ")
h6 <- createPlot(R6) + ggtitle("6 affected") + labs(y = " ")
#h8 <- createPlot(R8) + ggtitle("8 affected")

# Violins
v2 <- create_violin(r2$BET) + ggtitle(" ") + 
  labs(y = expression(paste("Median ", beta["id"]) ))
v4 <- create_violin(r4$BET) + ggtitle(" ") + labs(y = " ")
v6 <- create_violin(r6$BET) + ggtitle(" ") + labs(y = " ")
#v8 <- create_violin(r8$BET)  + ggtitle("8 affected")

plot <- ggarrange(h2,h4,h6,v2,v4,v6)
