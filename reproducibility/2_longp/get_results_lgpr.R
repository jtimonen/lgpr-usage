REL <- matrix(0, 200, 12)
for(idx in 1:200){
  fn <- paste("res/res_lgpr_", idx, ".rds", sep ="")
  res <- readRDS(fn)
  rel <- res$fit@covariate_relevances$average
  REL[idx, ] <- rel
  cat("idx=",idx,"\n",sep="")
  print(rel)
}

colnames(REL) <- names(rel)
write.table(REL, file = "relevances_lgpr.txt")
