
res <- readRDS('roc_noise.rds')

auc1 <- rep(0, 8)
auc2 <- rep(0, 8)
for (j in 1:8) {
  r <- res[[j]]$ROC
  auc1[j] <- r[[1]]$auc
  auc2[j] <- r[[2]]$auc
}

snr <- c(0.05, 0.1, 0.5, 1, 2, 5, 10, 20)
plot(snr, auc1, 'o', ylim = c(0,1), 'ylab' = "AUC", 
     xlab = 'Signal-to-noise ratio', pch = 16, xlim = c(0, 10))
lines(snr, auc2, col = "firebrick3")
points(snr, auc2, col = "firebrick3", pch = 16)
