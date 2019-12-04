#!/usr/bin/env Rscript
require(ggplot2)
require(ggpubr)

# 1) Read lonGP result table
fn  <- paste("post/longp.txt")
SEL <- read.table(fn, header = FALSE, sep=",")

# 2) Read lgpr result table
fn  <- paste("post/lgpr.txt")
REL <- read.table(fn, header = TRUE, sep=" ")

# 3) Compute selected covariates with certain threshold
thresh  <- 0.8
SEL_lgpr <- matrix(0, 200, 12)
for(i in 1:200){
  prc <- REL[i,]
  prc <- as.numeric(prc)
  prc_base <- sum(prc[c(1,2,12)]) # id, age and noise are always selected
  srt   <- sort(prc[3:11], decreasing = T, index.return = T)
  n_sel <- sum(prc_base + cumsum(srt$x) <= thresh) + 1
  i_sel <- c(c(1,2,12), 2+srt$ix[1:n_sel])
  SEL_lgpr[i,i_sel] <- 1
}

# 4) Bar heights
INDS <- 3:11
h_a1 <- colMeans(SEL[1:100, INDS])
h_a2 <- colMeans(SEL_lgpr[1:100, INDS])
h_b1 <- colMeans(SEL[101:200, INDS])
h_b2 <- colMeans(SEL_lgpr[101:200, INDS])

# 5) Barplot for case when diseaseAge is relevant
covnames <- c("id", "age", "disAge", "x1", "x2", "x3", "x4", "z1", "z2", "z3", "z4")
covnames <- covnames[INDS]
L        <- length(covnames)
covnames <- rep(covnames, 2)
method   <- c(rep("LonGP", L), rep("lgpr", L))
scr      <- as.numeric(c(h_a1, h_a2))
df_a     <- data.frame(cbind(covnames, scr, method))
df_a$scr <- as.numeric(as.vector(df_a$scr))
colnames(df_a) <- c('Covariate', 'Score', 'Method')

# 6) Barplot for case when diseaseAge is relevant
scr  <- as.numeric(c(h_b1, h_b2))
df_b <- data.frame(cbind(covnames, scr, method))
df_b$scr <- as.numeric(as.vector(df_b$scr))
colnames(df_b) <- c('Covariate', 'Score', 'Method')

# 7) ggplot
pA <- ggplot(data=df_a, aes(x=Covariate, y=Score, fill=Method)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_fill_manual(values=c('#74add1','#d73027')) +
      labs(y = "Proportion of times selected") +
      theme_minimal() + ggtitle('Disease-related age relevant') +
      ylim(0,0.8)

pB <- ggplot(data=df_b, aes(x=Covariate, y=Score, fill=Method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c('#74add1','#d73027')) +
  labs(y = "Proportion of times selected") +
  theme_minimal() + ggtitle('Disease-related age not relevant') +
  ylim(0,0.8)

plot <- ggarrange(pA, pB, labels = c("a", "b"))

# Compute accuracies
LAB1 <- matrix(rep(c(1, 1,1,0,0, 1,1,0,0), 100), 100, 9, byrow = T)
LAB2 <- matrix(rep(c(0, 1,1,0,0, 1,1,0,0), 100), 100, 9, byrow = T)

ACC1 <- c(sum(SEL[1:100,INDS]==LAB1), sum(SEL_lgpr[1:100,INDS]==LAB1))/prod(dim(LAB1))
ACC2 <- c(sum(SEL[101:200,INDS]==LAB2), sum(SEL_lgpr[101:200,INDS]==LAB2))/prod(dim(LAB2))

# Print
df_acc <- data.frame(rbind(ACC1, ACC2))
colnames(df_acc) <- c("LonGP", "lgpr")
rownames(df_acc) <- c("disAge relevant", "disAge not relevant")
print(df_acc)

# Save as image files
#ggsave(pA, file = "post/pA.png")
#ggsave(pB, file = "post/pB.png") 
ggsave(plot, file = "post/bars.png", width = 8, height = 3)
ggsave(plot, file = "post/bars.eps", width = 8, height = 3)

