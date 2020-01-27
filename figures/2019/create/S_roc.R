require(pROC)
require(ggplot2)
require(ggpubr)
require(lgpr)
source('helper/roc.R')
source('helper/barplot.R')

# Get results
res1_100 <- readRDS('data/main2/roc/roc_lme4/roc_100.rds')
res1_300 <- readRDS('data/main2/roc/roc_lme4/roc_300.rds')
res1_600 <- readRDS('data/main2/roc/roc_lme4/roc_600.rds')
res2 <- readRDS('data/main2/roc/roc_nb/results_nb.rds')
res3 <- readRDS('data/main2/roc/roc_et/results_et.rds')
res4 <- readRDS('data/main2/roc/roc_heter/results_het.rds')

# CREATE ALL ROC
PAL <- 6
p1a <- roc_lme4(res1_100, PAL) 
p1b <- roc_lme4(res1_300, PAL)
p1c <- roc_lme4(res1_600, PAL)
p1  <- ggarrange(p1a, p1b, p1c, 
                 labels = c("N=100","N=300","N=600"),
                 nrow = 1, ncol = 3)

p2 <- roc_nb(res2, PAL)
L3 <- roc_et(res3, PAL)
p3a <- L3$a
p3b <- L3$b

L0  <- roc_heter_one(res4$rel[[1]], 0, PAL)
L2  <- roc_heter_one(res4$rel[[2]], 1, PAL)
L4  <- roc_heter_one(res4$rel[[3]], 1, PAL)
L6  <- roc_heter_one(res4$rel[[4]], 1, PAL)
L8  <- roc_heter_one(res4$rel[[5]], 1, PAL)

# ROC HETER
p4  <- ggarrange(L0$roc, L2$roc, L4$roc, L6$roc, L8$roc,
                 L0$beta, L2$beta, L4$beta, L6$beta, L8$beta,
                 nrow = 2, ncol = 5, labels = c("auto"))

# ROC EFFECT TIME


t <- seq(0, 96, by = 0.2)
y1 <- stats::dnorm(t, mean = 36, sd = 4)
y2 <- stats::dexp(t, rate = 0.05)
df <- data.frame(t, y1)
colnames(df) <- c("t", "Probability")
p3c <- ggplot(df, aes(x=t,y=Probability)) + geom_line() + xlim(0,96) +
  theme(panel.grid.minor.y = element_blank())
df <- data.frame(t, y2)
colnames(df) <- c("t", "Probability")
p3d <- ggplot(df, aes(x=t,y=Probability)) + geom_line() + xlim(0, 96) +
  theme(panel.grid.minor.y = element_blank())

pc <- ggarrange(p3c, p3d, labels = "auto", nrow = 2, ncol = 1)
p3 <- ggarrange(pc, p3a, p3b, labels = c(" ", "c", "d"), nrow = 1, ncol = 3)

