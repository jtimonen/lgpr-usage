
# Load raw count data
dat <- read.table(file = 'data/cd4_rawReadCounts_dataset.txt', 
                  sep = '\t', header = TRUE)

dat <- dat[6:dim(dat)[1],]

# Load gene lengths
GLEN <- read.table('data/ensemblGeneLengths.txt', header = T)

# Get gene lengths
G <- dim(dat)[1]
gene_lengths <- rep(0, G)
for(i in 1:G){
  gid <- as.character(dat[i,1])
  idx <- which(GLEN$EnsemblGeneID == gid)
  gene_lengths[i] <- as.numeric(GLEN$LongestTranscriptLength[idx])
  if(i %% 1000 == 0){
    cat(paste0("i = ", i, "\n"))
  }
} 

out <- list(dat=dat, gene_lengths=gene_lengths)
saveRDS(out, file = 'data/data_63677.rds')

