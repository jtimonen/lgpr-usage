
library(lgpr)
require(ggplot2)
require(ggpubr)
require(reshape2)
require(viridisLite)
require(MASS)

## Demonstrating additive GP components
set.seed(3219)


# function for plotting kernel matrices
plotMatrix <- function(A, title = "Kernel matrix"){
  
  A <- t(A)
  longData <- melt(A)
  
  h <- ggplot(longData, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradientn(colors = viridis(n= 256)) +
    labs(x="x", y="x'") +
    theme_bw() + theme(axis.text = element_blank(),
                       axis.ticks = element_blank())
  h <- h + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank())
  h <- h + theme(legend.position = c(0.75, 0.75)) + 
    theme(legend.title=element_text(size=7), legend.text=element_text(size=7)) +
    theme(legend.key.size = unit(0.4, "cm"))
  return(h)
}

plotFunctionDraw <- function(df, mu, K, K_eps, H, highlight = FALSE, gvar = NULL){
  f      <- mvrnorm(1, mu, K + K_eps)
  df$f   <- f
  if(!is.null(gvar)){
     df$grp  <- df[[gvar]]
  }
  if(highlight){
    if(is.null(gvar)){
      h  <- ggplot(df, aes(x = age, y = f, group = hl), size = 1) +
        geom_line(aes(color = hl), size = 1) 
    }else{
      h <- ggplot(df, aes(x = age, y = f, group = grp), size = 1) +
        geom_line(aes(color = hl), size = 1)
    }
  }else{
    h <- ggplot(df, aes(x = age, y = f, group = id), size = 1) +
      geom_line(size = 1)
  }
  h <- h + theme_bw()
  h <- h + theme(legend.position = c(0.75, 0.75)) + 
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) + ylim(-H,H) + 
    scale_x_continuous(breaks=c(0,12,24, 36, 48),
                       labels=c("0","12","24", "36", "48"))
  return(h)
}

plotRow <- function(df, mu, K, K_eps, H, hlvar, gvar = NULL){
  NCOL <- 5
  highlight <- !is.null(hlvar)
  if(highlight){
    df$hl <- as.factor(df[[hlvar]]) 
  }
  PLOTS <- list()
  PLOTS[[1]] <- plotMatrix(K)
  for(j in 2:NCOL){
    PLOTS[[j]] <- plotFunctionDraw(df, mu, K, K_eps, H, highlight, gvar)
  }
  p  <- ggarrange(plotlist = PLOTS, ncol = NCOL, nrow = 1)
  return(p)
}

# OPTIONS
H         <- 3
P         <- 100
N         <- 8
mu        <- rep(0, N*P)
K_eps     <- 1e-5*diag(N*P)
vm_params <- c(0.025, 1)
stp       <- 0.5

# CREATE X
simData <- simulate_data(N = N, 
                         t_data = seq(0, 48, length.out = P),
                         covariates = c(0,2,2),
                         n_categs = c(2,3),
                         lengthscales = c(12,24,1,12,12))

# CREATE DF
age    <- simData$data$age
id     <- simData$data$id
disAge <- simData$data$diseaseAge
sex    <- as.factor(simData$data$z1)
loc    <- as.factor(simData$data$z2)
group  <- as.numeric(!is.nan(disAge))
group  <- as.factor(group)
df     <- data.frame(id, age, disAge, loc, sex, group)

# COMPUTE KERNELS
Kmat       <- simData$kernel_matrices
xnn        <- as.numeric(!is.nan(disAge))
K_var_mask <- lgpr:::compute_K_var_mask(disAge, disAge, vm_params, stp)
ntime      <- length(xnn)/N
beta       <- c(1, 0.5, 0.3, 0.1)
caseID     <- rep(c(1,2,3,4,0,0,0,0), each = ntime)
K_beta     <- lgpr:::compute_K_beta(beta, caseID, caseID)
K_ns       <- lgpr:::kernel_bin(xnn, xnn) * 
  lgpr:::kernel_ns(disAge, disAge, 1, 1, a = stp, b = 0, c = 1)

KK <- list()
KK[[1]] <- Kmat[,,1]
KK[[2]] <- Kmat[,,2]
KK[[3]] <- Kmat[,,4]
KK[[4]] <- Kmat[,,5]
KK[[5]] <- K_ns
KK[[6]] <- K_var_mask * K_ns
KK[[7]] <- K_beta * KK[[6]]

# KERNELS FIGURE
NROW  <- 4
PLOTS <- list()
PLOTS[[1]] <- plotRow(df, mu, KK[[1]], K_eps, H, hlvar = NULL)
PLOTS[[2]] <- plotRow(df, mu, KK[[2]], K_eps, H, hlvar = NULL)
PLOTS[[3]] <- plotRow(df, mu, KK[[3]], K_eps, H, hlvar = "sex")
PLOTS[[4]] <- plotRow(df, mu, KK[[4]], K_eps, H, hlvar = "loc")

p <- ggarrange(plotlist = PLOTS, nrow = NROW, ncol = 1)

set.seed(4192)
# KERNELSE_DISEASE FIGURE
NROW  <- 4
PLOTS <- list()
PLOTS[[1]] <- plotRow(df, mu, KK[[5]], K_eps, H, hlvar = "group")
PLOTS[[2]] <- plotRow(df, mu, KK[[6]], K_eps, H, hlvar = "group")
PLOTS[[3]] <- plotRow(df, mu, KK[[7]], K_eps, H, hlvar = "group", gvar = "id")

p2 <- ggarrange(plotlist = PLOTS, nrow = NROW, ncol = 1)
