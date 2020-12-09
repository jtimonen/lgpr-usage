# Function for generating the test data
generate_data <- function(rel_da, N, by) {
   
   # Generate the covariates X
   simData <- simulate_data(N            = N,
                            t_data       = seq(12, 72, by = by),
                            covariates   = c(0, 1,1,1,1, 2,2,2,2),
                            t_effect_range = c(36,48),
                            t_jitter     = 1)
   X <- simData$data
   X["y"] <- NULL
   n <- nrow(X)
   
   # Define component types
   f_slow <- function(x, a) {sin(a - 0.25*x + exp(0.03*x))}
   f_fast <- function(x, x_cat, a, b, c) {
      f <- c + exp(-a*x)*sin(b*x)
      inds <- which(x_cat != 1)
      f[inds] <- -f[inds]
      return(f)
   }
   f_lin <- function(x) { x }
   f_sin <- function(x) { x - 2*x^2 + 0.5*exp(x**2) }
   f_peak <- function(x) {
      f <- exp(-0.01*x^2)
      inds <- which(is.nan(f))
      f[inds] <- 0
      return(f)
   }
   f_ofs <- function(x_cat) {
      # x must be integers here (ids)
      N <- length(unique(x_cat))
      offsets <- rnorm(N)
      return(offsets[x_cat])
   }
   
   # Normalization of effect size
   normalize <- function(f) { f/sd(f) }
   
   # Generate the components
   J <- 7
   FFF <- matrix(0, J, n)
   FFF[1,] <- f_ofs(X$id)
   FFF[2,] <- f_slow(X$age, 0)
   FFF[3,] <- f_peak(X$diseaseAge)
   FFF[4,] <- f_lin(X$x1)
   FFF[5,] <- f_sin(X$x2)
   FFF[6,] <- f_fast(X$age, X$z1, 0.02, 0.1, 1)
   FFF[7,] <- f_fast(X$age, X$z2, 0.02, 0.15, 0.5)
   for (j in 1:J) {
      FFF[j,] <- normalize(FFF[j,])
   }
   FFF[3,] <- rel_da*FFF[3,] # is disease-age relevant?
   
   # Generate response
   f <- colSums(FFF)
   f <- normalize(f)
   y <- f + sqrt(1/3)*rnorm(n)
   
   data <- data.frame(cbind(X,y))
   ret <- list(data = data, comp = FFF)
   return(ret)
}
