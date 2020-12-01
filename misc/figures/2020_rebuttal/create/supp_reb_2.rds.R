
res <- readRDS('roc_nonlin.rds')

auc1 <- rep(0, 6)
auc2 <- rep(0, 6)
for (j in 1:6) {
  r <- res[[j]]$ROC
  auc1[j] <- r[[1]]$auc
  auc2[j] <- r[[2]]$auc
}

B <- c(0.02, 0.04, 0.06, 0.08, 0.1, 0.2)
plot(B, auc1, 'o', ylim = c(0,1), 'ylab' = "AUC", 
     xlab = 'Signal-to-noise ratio', pch = 16, xlim = c(0, 0.2))
lines(B, auc2, col = "firebrick3")
points(B, auc2, col = "firebrick3", pch = 16)
