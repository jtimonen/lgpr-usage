require(lgpr)
cat('Start...')

REL <- matrix(0, 200, 12)
P99 <- matrix(0, 200, 12)
P95 <- matrix(0, 200, 12)
P90 <- matrix(0, 200, 12)
P85 <- matrix(0, 200, 12)
P80 <- matrix(0, 200, 12)

for(idx in 1:200){
  fn <- paste("res/lgpr_", idx, ".rds", sep ="")
  cat('Loading ', fn, '...', sep = "")
  res <- readRDS(fn)
  fit <- res$fit
  
  rel <- fit@relevances$average
  p99 <- selection(fit, 0.99)$prob
  p95 <- selection(fit, 0.95)$prob
  p90 <- selection(fit, 0.90)$prob
  p85 <- selection(fit, 0.85)$prob
  p80 <- selection(fit, 0.80)$prob
  
  REL[idx,] <- rel
  P99[idx,] <- p99
  P95[idx,] <- p95
  P90[idx,] <- p90
  P85[idx,] <- p85
  P80[idx,] <- p80

  cat("idx=",idx,"\n",sep="")
  print(rel)
}

colnames(REL) <- names(rel)
colnames(P99) <- names(p99)
colnames(P95) <- names(p95)
colnames(P90) <- names(p90)
colnames(P85) <- names(p85)
colnames(P80) <- names(p80)

res <- list(REL = REL, P99 = P99, P95 = P95, P90 = P90,
            P85 = P85, P80 = P80)

saveRDS(res, 'sel/res_lgpr.rds')
#write.table(REL, file = "sel/lgpr_rel.txt")
