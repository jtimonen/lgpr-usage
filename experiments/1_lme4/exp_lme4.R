#!/usr/bin/env Rscript

# This R script is meant to be called by a shell script with an integer argument
# For example:
#   n=$SLURM_ARRAY_TASK_ID
#   srun Rscript --vanilla exp_lme4.R $n

args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
require(lgpr)
require(lme4)

# Define a function that runs the experiment
runExperiment <- function(idx){
  
 num_idx <- as.numeric(idx)
 if(num_idx <= 100){
   relev <- c(1,1,0,0)
 }else if(num_idx <= 200){
   relev <- c(1,1,0,1)
 }else{
   relev <- c(1,1,1,1)
 }
 print(relev)

 cat("This is experiment number ", idx, "\n", sep ="")
 simData <- simulate_data(N           = 14,
                         t_data       = c(6,12,18,24,36),
                         covariates   = c(2,2),
                         relevances   = relev,
                         n_categs     = c(2,3),
                         names        = c("sex", "location"),
                         lengthscales = c(6,12,6,6),
                         t_jitter     = 0.2)
 data <- simData$data

 # print crosstabulation
 xt <- xtabs(~ sex + location, data)
 print(xt)

 # lme4
 fit_lmer <- lmer(y ~ 1 + id + age + (age | id) 
                 + sex + location, data = data)
 ANOVA <- anova(fit_lmer)
 print(fit_lmer)
 print(ANOVA)
 fval <- ANOVA$`F value`
 names(fval) <- rownames(ANOVA)
 
 # lgpr
 fit_lgp <- lgp(y ~ id + age + sex + location, 
                  data    = data,
                  iter    = 800, 
                  chains  = 3,
                  refresh = 1000)

  # Return results
  res_small <- list(lgpr  = fit_lgp@covariate_relevances$average,
                    fval  = fval,
                    real  = relev,
                    idx   = idx)
  return(res_small)
}


# Run the experiment
res <- runExperiment(idx_in)
save(res, file = paste("res/res_",idx_in,sep=""))



