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
plots_b <- list()
j <- 0
for (idx in INDS) {
  j <- j + 1
  r <- res[[idx]]$ROC
  plt_a <- create_roc_plot(r[[1]], r[[2]])
  plt_b <- create_rel_plot(res[[idx]], cnames)
  plt_a <- plt_a + ggtitle(paste0('SNR = ', snr[idx]))
  plt_b <- plt_b + ggtitle(paste0('SNR = ', snr[idx]))
  plots_a[[j]] <- plt_a
  plots_b[[j]] <- plt_b
}

plt_a <- ggarrange(plotlist = plots_a, labels = "auto")
plt_b <- ggarrange(plotlist = plots_b, labels = "auto")
