require(ggplot2)
require(ggpubr)

source('helpers.R')
cnames <- c("f_1(idxage)", "f_2(age)", 
            "f_3(z1, age)", "f_4(z2, age)", "f_5(z3, age)",
            "noise")

# Load results
res <- readRDS('res/res_noise.rds')
snr <- c(0.05, 0.1, 0.5, 1, 2, 3, 4, 5)
INDS <- c(2,3,4,5,6)
L <- length(snr)
plots_a <- list()
j <- 0
S <- 100
rel_noise <- matrix(0, S, length(INDS))
colnames(rel_noise) <- snr[INDS]

# Create first plots
for (idx in INDS) {
  j <- j + 1
  r <- res[[idx]]$ROC
  plt_a <- create_roc_plot(r[[1]], r[[2]])
  plt_a <- plt_a + ggtitle(paste0('SNR = ', snr[idx]))
  plots_a[[j]] <- plt_a
  rel_noise[,j] <- get_noise_rel(res[[idx]], 6)
}

plt_a <- ggarrange(plotlist = plots_a, labels = "auto", nrow = 2, ncol = 3)

# Create inferred snr plot
a <- as.numeric(rel_noise)
inferred_snr <- (1 - a) / a
b <- rep(snr[INDS], each = S)
df <- data.frame(as.factor(b), inferred_snr)
colnames(df) <- c("true", "inferred")
aesth <- aes(x = true, y = inferred)
plt_b <- ggplot(df, aesth) + geom_violin(fill = "steelblue")

plt <- ggarrange(plt_a, plt_b, nrow = 2)

