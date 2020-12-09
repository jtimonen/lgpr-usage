require(lgpr)
library(ggplot2)
library(ggpubr)

# Define helper function
add_num_label <- function(x) {
  nam <- names(x)
  prefixes <- formatC(1:length(nam), width = 2, flag = "0")
  nam <- paste(prefixes, nam, sep = "_")
  names(x) <- nam
  return(x)
}

RES_FIELD <- c("R1", "R2", "R3")
nam_A <- "n_96"
nam_B <- "n_216"
plots <- list()

# Load inference results
res <- readRDS('res/res_mismatch_large.rds')

for (idx in 1:3) {
  # Get average inferred relevances
  rf <- RES_FIELD[idx]
  rel_A <- res[[nam_A]]
  rel_B <- res[[nam_B]]
  bar_A <- apply(rel_A[[rf]], 2, mean)
  bar_B <- apply(rel_B[[rf]], 2, mean)
  err_A <- apply(rel_A[[rf]], 2, stats::sd)
  err_B <- apply(rel_B[[rf]], 2, stats::sd)
  bar_A <- add_num_label(bar_A)
  bar_B <- add_num_label(bar_B)
  
  # True relevances
  p_noise <- 1 / (3 + 1)
  p_signal <- 1 - p_noise
  if (idx == 1) {
    v1 <- p_signal / 7
    bar_C <- c(v1,v1,v1,v1,v1,0,0,v1,v1,0,0, p_noise)
  } else if (idx == 2) {
    v1 <- p_signal / 5
    bar_C <- c(v1,v1,v1,v1,0,0,v1,0,0, p_noise)
  } else if (idx == 3) {
    v1 <- p_signal / 4
    bar_C <- c(v1,v1,v1,0,0,v1,0,0, p_noise)
  } else {
    stop("idx must be 1, 2 or 3!")
  }
  
  # Plot setup
  nams <- names(bar_A)
  values <- c(bar_A, bar_B)
  stds <- c(err_A, err_B)
  lab <- rep(c("096", "216"), each = length(nams))
  cname <- rep(nams, length(values))
  df <- data.frame(values, stds, as.factor(lab), as.factor(cname))
  colnames(df) <- c("relevance", "sd", "n", "component")
  
  # Create relevance bar plot
  plt <- ggplot(df, aes(x = component, y = relevance, group = n, fill = n)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_brewer(palette = "Paired") + ylim(0, 0.55)
  
  # Add standard deviation
  #plt <- plt + geom_errorbar(aes(ymin=relevance-sd, ymax=relevance+sd), width=.2,
  #                           position=position_dodge(.9))
  
  plots[[idx]] <- plt
}

plt <- ggarrange(plotlist = plots, ncol = 2, nrow = 2, labels = "auto")
