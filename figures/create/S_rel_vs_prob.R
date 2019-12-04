require(pROC)
require(ggplot2)
require(ggpubr)
require(lgpr)
source('roc.R')

# Get results
res <- readRDS('data/main2/roc/roc_longp/res_lgpr.rds')
r1 <- c(1,1,1, 1,1,0,0, 1,1,0,0)
r2 <- c(1,1,1, 1,1,0,0, 1,1,0,0)
real <- c(rep(r1, 100), rep(r2, 100))
PRED <- matrix(0, length(real), 6)
icol <- 1:11
PRED[,1] <- as.numeric(t(res$REL[,icol]))
PRED[,2] <- as.numeric(t(res$P99[,icol]))
PRED[,3] <- as.numeric(t(res$P95[,icol]))
PRED[,4] <- as.numeric(t(res$P90[,icol]))
PRED[,5] <- as.numeric(t(res$P85[,icol]))
PRED[,6] <- as.numeric(t(res$P80[,icol]))
colnames(PRED) <- c("lpgr-rel", 
                    "lgpr-prob (T=0.99)",
                    "lgpr-prob (T=0.95)",
                    "lgpr-prob (T=0.90)",
                    "lgpr-prob (T=0.85)",
                    "lgpr-prob (T=0.80)")
PRED <- data.frame(PRED)
PRED <- PRED[,c(1,4,6)]
# Create figure
df <- create_roc_df(real, PRED)
plt <- create_roc_plot(df)

#theme(legend.position = c(0.6,0.4), 
##      legend.text=element_text(size=rel(0.75)), legend.title=element_blank()) +
#  scale_colour_brewer(type = "qual", palette = "Set1", direction = 1,
#                      aesthetics = "colour")