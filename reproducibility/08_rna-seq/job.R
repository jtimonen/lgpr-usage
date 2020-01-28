#!/usr/bin/env Rscript
args   <- commandArgs(trailingOnly=TRUE)
idx_in <- args[1]
N_ITER <- 4000
ADAPT_DELTA <- 0.99

require(lgpr)
require(ggplot2)
 
#Function for reading data
source('preproc/04_create_df.R')

# Control parameters
CNTR  <- list(adapt_delta = ADAPT_DELTA)

# Create fig dir
i_gene <- as.numeric(idx_in)
figDir <- paste0('figs/figs_', i_gene)
dir.create(figDir)

# Read data frame from file
dat   <- readRDS(file='../data/rna-seq/data_519.rds')
cat(paste0('gene index = ', i_gene, '\n'))
input <- create_df(i_gene, dat)
nf    <- input$norm_fac
data  <- input$df
gnam  <- dat$gene_names[i_gene]
cat(paste0('gene_name:', gnam, '\n'))
 
# Set normalization
C_hat <- adjusted_Chat(data$y, input$norm_fac)
#print(C_hat)

# Fit model
fit <- lgp(formula = y ~ id + age + diseaseAge + group + sex,
           data = data,
           likelihood = "nb",
           C_hat = C_hat,
           iter = N_ITER,
           offset = "group",
           chains = 4,
           control = CNTR,
           save_warmup = FALSE,
           verbose = TRUE)

#print(fit@diagnostics)

# Return results
results <- list(data = data, fit = fit, n_iter = N_ITER, control = CNTR, gene_name = gnam)
save(results, file = paste("res/res_", i_gene, sep=""))

# Plots
FONT_SIZE <- 14
p1     <- plot_components_posterior(fit, font_size = FONT_SIZE)
fn1 <- paste(figDir, "/c.png", sep="")
ggsave(filename = fn1, plot = p1, width = 8, height = 5.5,  units = "in")

