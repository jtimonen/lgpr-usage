#function (sigma_alpha = 1) 
#{
#  pmag <- list(type = "student-t", mu = 0, sigma = sigma_alpha, 
#               nu = 20)
#  Distribution_mag <- list(idAge = pmag, sharedAge = pmag, diseaseAge = pmag, 
#                    continuous = pmag, categorical = pmag, offset = pmag)
#  plen <- list(type = "log-normal", mu = 0, sigma = 1)
#  Distribution_ls <- list(idAge = plen, sharedAge = plen, diseaseAge = plen, 
#                   continuous = plen, categorical = plen)
#  Distribution_st <- list(type = "inv-gamma", shape = 14, scale = 5)
#  Distribution_sig <- list(type = "inv-gamma", shape = 2, scale = 1, 
#                    transform = "square")
#  Distribution_phi <- list(type = "log-normal", mu = 1, sigma = 1, 
#                    transform = "square")
#  Distribution_bet <- list(shape1 = 0.2, shape2 = 0.2)
#  Distribution_ons <- list(type = "uniform_whole")
#  Distribution <- list(alpha = Distribution_mag, lengthscale = Distribution_ls, 
#                warp_steepness = Distribution_st, sigma_n = Distribution_sig, phi = Distribution_phi, 
#                beta = Distribution_bet, t_effect = Distribution_ons, vm_params = c(0.025, 
#                                                                      1))
#  return(Distribution)
#}

require(ggplot2)
require(stats)
require(ggpubr)

my_dinvgamma <- function(x, alpha, beta, log = FALSE){
  logm1 <- alpha*log(beta) - lgamma(alpha)
  logm2 <- -(alpha+1)*log(x)
  logm3 <- -beta*1/x
  out   <- logm1 + logm2 + logm3 
  if(!log){
    out <- exp(out)
  }
  return(out)
}

h <- 0.01
h2 <- 0.05

# Magnitude default Distribution
t     <- seq(0, 3, by = h)
t2    <- seq(0, 20, by = h2)

p1    <- dt(t, df = 20, log = FALSE)
logp1 <- dt(t2, df = 20, log = TRUE)

# Lengthscale default Distribution
p2    <- dlnorm(t, meanlog = 0, sdlog = 1, log = FALSE)
logp2 <- dlnorm(t2, meanlog = 0, sdlog = 1, log = TRUE)

# Steepness default Distribution
p3    <- my_dinvgamma(t, alpha = 14, beta = 5, log = FALSE)
logp3 <- my_dinvgamma(t2, alpha = 14, beta = 5, log = TRUE)

# Create plots A and B
name  <- c("Student-t(20)", "Log-Normal(0,1)", "Inv-Gamma(14,5)")
names <- rep(name, each = length(t))
names2 <- rep(name, each = length(t2))

df    <- data.frame(names, rep(t, length(name)), c(p1, p2, p3))
colnames(df) <- c("Distribution", "x", "px")
plt_a <- ggplot(df, aes(x = x , y = px, group = Distribution, color = Distribution)) + 
  geom_line() +
  theme_bw() + theme(legend.position = c(0.7,0.4)) + ylab('p(x)')

df    <- data.frame(names2, rep(t2, length(name)), c(logp1, logp2, logp3))
colnames(df) <- c("Distribution", "x", "logpx")
plt_b <- ggplot(df, aes(x = x , y = logpx, group = Distribution, color = Distribution)) +
  geom_line() + ylim(-15,3) +
  theme_bw() + theme(legend.position = c(0.7,0.4)) + ylab('log p(x)')


# sqrt(sigma) default distribution
p4    <- my_dinvgamma(t, alpha = 2, beta = 1, log = FALSE)
logp4 <- my_dinvgamma(t2, alpha = 2, beta = 1, log = TRUE)

# sqrt(phi) default distribution
p5    <- dlnorm(t, meanlog = 1, sdlog = 1, log = FALSE)
logp5 <- dlnorm(t2, meanlog = 1, sdlog = 1, log = TRUE)

# Create plots C and D
name  <- c("Inv-Gamma(2,1)", "Log-Normal(1,1)")
names <- rep(name, each = length(t))
names2 <- rep(name, each = length(t2))

df    <- data.frame(names, rep(t, length(name)), c(p4, p5))
colnames(df) <- c("Distribution", "x", "px")
plt_c <- ggplot(df, aes(x = x , y = px, group = Distribution, color = Distribution)) + 
  geom_line() +
  theme_bw() + theme(legend.position = c(0.7,0.4)) + ylab('p(x)')

df    <- data.frame(names2, rep(t2, length(name)), c(logp4, logp5))
colnames(df) <- c("Distribution", "x", "logpx")
plt_d <- ggplot(df, aes(x = x , y = logpx, group = Distribution, color = Distribution)) + 
  geom_line() + ylim(-15,3) +
  theme_bw() + theme(legend.position = c(0.7,0.4)) + ylab('log p(x)')

# beta default distribution
t3    <- seq(0.01,0.99,by=0.001)
p6    <- dbeta(t3, shape1 = 0.2, shape2 = 0.2, log = FALSE)
logp6 <- dbeta(t3, shape1 = 0.2, shape2 = 0.2, log = TRUE)

# Create plots C and D
name  <- c("Beta(0.2, 0.2)")
names <- rep(name, each = length(t3))

df    <- data.frame(names, rep(t3, length(name)), c(p6))
colnames(df) <- c("Distribution", "x", "px")
plt_e <- ggplot(df, aes(x = x , y = px, group = Distribution, color = Distribution)) +
  geom_line() +
  theme_bw() + theme(legend.position = c(0.7,0.4)) + ylab('p(x)')

df    <- data.frame(names, rep(t3, length(name)), c(logp6))
colnames(df) <- c("Distribution", "x", "logpx")
plt_f <- ggplot(df, aes(x = x , y = logpx, group = Distribution, color = Distribution)) +
  geom_line() + ylim(-2,2) +
  theme_bw() + theme(legend.position = c(0.7,0.4)) + ylab('log p(x)')


col1 <- ggarrange(plt_a, plt_b, ncol = 1, nrow = 2)
col2 <- ggarrange(plt_c, plt_d, ncol = 1, nrow = 2)
col3 <- ggarrange(plt_e, plt_f, ncol = 1, nrow = 2)
plt  <- ggarrange(col1, col2, col3, nrow = 1, ncol = 3, labels = "auto")
