library(edgeR)

# Load filtered data with normalization factors
data     <- readRDS('data/data_10454.rds')
norm_fac <- data$X$samples$norm.factors
counts   <- data$X$counts
tpm      <- data$tpm

# Compute coefficients of variation
mu    <- rowMeans(tpm)
sigma <- apply(tpm, 1, stats::sd)
cv    <- sigma/mu

# Select highly variable genes
i_sel <- which(cv > 0.75)
x     <- counts[i_sel,]
G     <- dim(x)[1]

# Get gene names
GNAM <- read.table('data/ensemblIds_withGeneNames_57392.txt', header = FALSE)
colnames(GNAM) <- c("ENSG", "name")
gene_names <- rep("foo", G)
for(i in 1:G){
  ensg <- rownames(x)[i]
  idx <- which(GNAM$ENSG==ensg)
  if(length(idx)==0){
    gene_names[i] <- NA
  }else{
    gene_names[i] <- as.character(GNAM$name[idx]) 
  }
}

out <- list(x=x, gene_names=gene_names, norm_fac=norm_fac)
saveRDS(out, file = "data/data_519.rds")

