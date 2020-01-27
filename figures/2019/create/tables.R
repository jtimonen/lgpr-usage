
res1 <- readRDS(file = 'tables/res_basic.rds')
res2 <- readRDS(file = 'tables/res_heter.rds')

for(i in 1:dim(res1)[1]){
  rel <- res1[i,]
}
R1 <- res1
R2 <- res2
R1[,4:10] <- round(R1[,4:10], 3)
R2[,4:10] <- round(R2[,4:10], 3)

# Create Supplementary Tables 1-2
#write.csv(R1, file = "tables/basic.csv")
#write.csv(R2, file = "tables/heter.csv")

# Create combined df for comparing
COMB <- res1[,c(1,2,4:9)]
COMB <- cbind(COMB, res2[c(4:9,13,15)])
colnames(COMB) <- c("idx", "name", 
                    "id1", "age1", "dAge1", "sex1", "grp1", "noise1",
                    "id2", "age2", "dAge2", "sex2", "grp2", "noise2",
                    "N_cases", "N_aff")

#saveRDS(COMB, file = 'tables/res_combined.rds')
