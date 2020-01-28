library(lgpr)
source('04_create_df.R')

# Read data frame from file
dat   <- readRDS(file='data/data_519.rds')
idx   <- 7
input <- create_df(idx, dat)
nf    <- input$norm_fac
data  <- input$df
gnam  <- dat$gene_names[idx]

# Plot data
h <- plot_data(data, highlight = "diseaseAge", title = gnam)
h
