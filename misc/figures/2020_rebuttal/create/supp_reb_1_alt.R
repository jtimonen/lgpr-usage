require(ggplot2)

# Load results
res <- readRDS('res/roc_noise.rds')
L <- 6
auc1 <- rep(0, L)
auc2 <- rep(0, L)
for (j in 1:L) {
  r <- res[[j]]$ROC
  auc1[j] <- r[[1]]$auc
  auc2[j] <- r[[2]]$auc
}
snr <- c(0.05, 0.1, 0.5, 1, 2, 5)

# Create plot data frame
AUC <- c(auc1, auc2)
SNR <- rep(snr, 2)
Method <- rep(c("lgpr", "lme4"), each = L)
df <- data.frame(SNR, AUC, as.factor(Method))
colnames(df) <- c("SNR", "AUC", "Method")

# Create plot
aesth <- aes(x = SNR, y = AUC, color = Method, group = Method)
plt <- ggplot(df, aesth) + geom_line() + geom_point()
