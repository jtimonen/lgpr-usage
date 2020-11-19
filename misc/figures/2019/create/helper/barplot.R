

bplots <- function(fit){
  p <- fit@selection$prob
  p <- p[1:(length(p)-1)]
  df <- data.frame(as.factor(names(p)), p)
  colnames(df) <- c("Component", "Probability")
  gg1 <- ggplot(df, aes(x=Component, y=Probability)) + 
    geom_bar(stat="identity") + theme_bw()
  
  gg2 <- plot_relevances(fit, outlier.size = 0, outlier.stroke=0) + theme_bw()
  gg1 <- gg1 + ggtitle('Prob')
  gg <- list(gg1, gg2)
  return(gg)
}