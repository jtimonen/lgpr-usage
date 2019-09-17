#!/usr/bin/env Rscript

require(pROC)
require(ggplot2)
require(ggpubr)

# Path to the location where the results are
parentDir <- "res"

# A result loader function
get_res <- function(parentDir, n_aff){

  if(n_aff==2){
    offs <- 100
  }else if(n_aff==4){
    offs <- 200
  }else if(n_aff==6){
    offs <- 300
  }else if(n_aff==8){
    offs <- 400
  }else{
    offs <- 0
  }

  idata <- offs + 1:100
  L     <- length(idata)
  REL   <- array(0, c(L,7,2))
  BET   <- matrix(0, L, 8)
  j     <- 0
  for(idx in idata){
      j  <- j + 1
      fn <- paste(parentDir, "/03_naff_", n_aff, "_", idx, sep ="")
      load(fn)
      REL[j,,1] <- res$r1
      REL[j,,2] <- res$r2
      BET[j,]   <- res$beta$medians
  }
  return(list(REL=REL,BET=BET))
}

# Compute ROC
compute_ROC <- function(res, rel_da){
  real  <- c(1,1,rel_da, 1,0,0)
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
create_boxplot <- function(BETA){
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
r0 <- get_res(parentDir, 0)
r2 <- get_res(parentDir, 2)
r4 <- get_res(parentDir, 4)
r6 <- get_res(parentDir, 6)
r8 <- get_res(parentDir, 8)

# Compute ROC
R0 <- compute_ROC(r0, 0)
R2 <- compute_ROC(r2, 1)
R4 <- compute_ROC(r4, 1)
R6 <- compute_ROC(r6, 1)
R8 <- compute_ROC(r8, 1)

# Plot ROC
h0 <- createPlot(R0) + ggtitle("0 affected")
h2 <- createPlot(R2) + ggtitle("2 affected") + labs(y = " ")
h4 <- createPlot(R4) + ggtitle("4 affected") + labs(y = " ")
h6 <- createPlot(R6) + ggtitle("6 affected") + labs(y = " ")
h8 <- createPlot(R8) + ggtitle("8 affected")

# Violins
v0 <- create_boxplot(r0$BET) + ggtitle(" ") +
  labs(y = expression(paste("Median ", beta["id"]) ))
v2 <- create_boxplot(r2$BET) + ggtitle(" ") +
  labs(y = " ") 
  #labs(y = expression(paste("Median ", beta["id"]) ))
v4 <- create_boxplot(r4$BET) + ggtitle(" ") + labs(y = " ")
v6 <- create_boxplot(r6$BET) + ggtitle(" ") + labs(y = " ")
v8 <- create_boxplot(r8$BET) + ggtitle("8 affected")

plot <- ggarrange(h0,h2,h4,h6,h8, 
                  v0,v2,v4,v6,v8,
                  nrow = 2,
                  ncol = 5,
                  labels = c("a", "b", "c", "d", "e",
                             "f", "g", "h", "i", "j"))

ggsave(plot, file = "post/panels.eps", width = 12.5, height = 5.25)

