require(pROC)
require(ggplot2)
require(ggpubr)
require(lgpr)
source('helper/roc.R')
source('helper/barplot.R')



# lme4 + lmerTest ---------------------------------------------------------


# Get results
#res1_100 <- readRDS('data/main2/roc/roc_lme4/roc_100.rds')
#res1_300 <- readRDS('data/main2/roc/roc_lme4/roc_300.rds')
#res1_600 <- readRDS('data/main2/roc/roc_lme4/roc_600.rds')

# CREATE LME4 ROC
#PAL <- 6
#p1a <- roc_lme4(res1_100, PAL) 
#p1b <- roc_lme4(res1_300, PAL)
#p1c <- roc_lme4(res1_600, PAL)
#p1  <- ggarrange(p1a, p1b, p1c, 
#                 labels = c("N=100","N=300","N=600"),
#                 nrow = 1, ncol = 3)


# lonGP -------------------------------------------------------------------

lgpr_select <- function(rel, thresh) {
  sel <- rep(0, 11)
  isel <- lgpr:::selection_fixed_threshold(rel, thresh)
  isel <- isel[2:length(isel)]
  sel[isel] <- 1
  return(sel)
}

# Get lgpr selections with thresholds 0.8 and 0.95
res   <- readRDS('data/supp_longp/res_lgpr.rds')
icol  <- 1:11
SEL_A <- matrix(0, 200, 11)
SEL_C <- matrix(0, 200, 11)
REL   <- res$REL
for (i in 1:dim(SEL_A)[1]){
  SEL_A[i, ] <- lgpr_select(REL[i,], 0.80)
  SEL_C[i, ] <- lgpr_select(REL[i,], 0.95)
}

# Get lonGP selections
SEL_B <- read.csv(file = 'data/supp_longp/selected_lonGP.txt', header = FALSE)

# Create figures
cind   <- c(3:11)
SEL_A1 <- SEL_A[1:100, cind]
SEL_B1 <- SEL_B[1:100, cind]
SEL_C1 <- SEL_C[1:100, cind]
SEL_A2 <- SEL_A[101:200, cind]
SEL_B2 <- SEL_B[101:200, cind]
SEL_C2 <- SEL_C[101:200, cind]

bars1a <- colMeans(SEL_A1)
bars1b <- colMeans(SEL_B1)
bars1c <- colMeans(SEL_C1)
bars2a <- colMeans(SEL_A2)
bars2b <- colMeans(SEL_B2)
bars2c <- colMeans(SEL_C2)

cnams  <- c("id", "age", "diseaseAge", 
            "x1", "x2", "x3", "x4",
            "z1", "z2", "z3", "z4")
Covariate <- as.factor(rep(cnams[cind], 3))
Method    <- as.factor(rep(c("lgpr (T = 0.80)", 
                             "lonGP", 
                             "lgpr (T = 0.95)"), 
                           each = length(cind)))

# Create figure A (disease is relevant)
Prob1 <- c(bars1a, bars1b, bars1c)
df1   <- data.frame(Method, Covariate, Prob1)
p1    <- ggplot(df1, aes(x=Covariate, 
                             y=Prob1, 
                             group=Method,
                             fill=Method)) +
  ggplot2::geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() + ggtitle('Disease-age relevant') +
  ylab('Proportion of times selected') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0,1)

# Create figure B (disease is not relevant)
Prob2      <- c(bars2a, bars2b, bars2c)
df2       <- data.frame(Method, Covariate, Prob2)
p2        <- ggplot(df1, aes(x=Covariate, 
                             y=Prob2, 
                             group=Method,
                             fill=Method)) +
  ggplot2::geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() + ggtitle('Disease-age not relevant') +
  ylab('Proportion of times selected') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0,1)

full <- ggarrange(p1, p2, nrow = 1, ncol = 2, labels = "auto")

# Accuracies
Real1 <- matrix(rep(c(1, 1,1,0,0, 1,1,0,0), 100), 100, 9, byrow = TRUE)
Real2 <- matrix(rep(c(0, 1,1,0,0, 1,1,0,0), 100), 100, 9, byrow = TRUE)
Accuracy_1a <- sum(SEL_A1 == Real1)/(900)
Accuracy_1b <- sum(SEL_B1 == Real1)/(900)
Accuracy_1c <- sum(SEL_C1 == Real1)/(900)
Accuracy_2a <- sum(SEL_A2 == Real2)/(900)
Accuracy_2b <- sum(SEL_B2 == Real2)/(900)
Accuracy_2c <- sum(SEL_C2 == Real2)/(900)


str1 <- paste0('\nAccuracies (diseaseAge relevant):\n',
          '   lgpr (0.80): ', round(Accuracy_1a, 3), '\n',
          '   lonGP: ', round(Accuracy_1b, 3), '\n',
          '   lgpr (0.95): ', round(Accuracy_1c, 3), '\n')

str2 <- paste0('\nAccuracies (diseaseAge not relevant):\n',
               '   lgpr (0.80): ', round(Accuracy_2a, 3), '\n',
               '   lonGP: ', round(Accuracy_2b, 3), '\n',
               '   lgpr (0.95): ', round(Accuracy_2c, 3), '\n')

# Runtimes
str3 <- paste0('\nRuntimes: \n',
               '   lgpr: 58 min 21 s (+- 1 min 33 s) \n',
               '   lonGP: 4 h 40 min 11 s (+- 1h 43 min 10 s) \n')

str <- paste(str1, str2, str3)
cat(str)

