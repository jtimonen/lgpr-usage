library(edgeR)

input <- readRDS('data/data_63677.rds')
dat   <- input$dat

G    <- dim(dat)[1]
N    <- dim(dat)[2]
ENSG <- as.character(input$dat[,1])
x    <- as.matrix(input$dat[,2:N]) # remove row names
rownames(x) <- ENSG

# Compute TPM
lkb <- input$gene_lengths/1000
RPK <- x/lkb              # reads per kilobase
pms <- colSums(RPK)/(1e6) # per million scaling factor
PMS <- matrix(rep(pms, G), nrow=G, ncol=72, byrow = TRUE)
TPM <- RPK/PMS

# Filter out genes for which TPM==0 in more than 50% of measurements
a      <- rowSums(TPM > 0)
i_keep <- which(a  >= (72/2))
TPM    <- TPM[i_keep,] # left with 21,314 genes

# Filter out genes for which mean(TPM) <= 3
i_keep <- which(rowMeans(TPM) > 3)
TPM    <- TPM[i_keep,]
i_sel  <- which(rownames(x) %in% rownames(TPM))
x      <- x[i_sel,]

# Compute normalization factors with edgeR
X <- DGEList(counts=x)
X <- calcNormFactors(X)

# Save
out <- list(X=X, tpm=TPM)
saveRDS(out, file = "data/data_10454.rds")
