require(ggplot2)
require(ggpubr)
require(lgpr)

# create heterogeneous component inferences to main 2 ---------------------

all     <- readRDS(file = 'data/main2/prot/interesting.rds') # 1.3 Gb
fit1    <- all$heter_1343
fit2    <- all$basic_1343
prot_name <- "Q8WZA1"
#n_smp  <- 4*1/2*fit1@stan_fit@stan_args[[1]]$iter
#i_smp  <- sample.int(n_smp, 400, replace = FALSE)
t_test <- seq(0, 180, 1)
X_test <- create_test_points(fit1, t_test)
PRED1  <- lgp_predict(fit1, X_test, samples = "mean")
PRED2  <- lgp_predict(fit2, X_test, samples = "mean")
#PREDS   <- readRDS('data/main2/prot/preds_1343_400_samples.rds')
#PRED1   <- PREDS[[1]]
#PRED1$LIST <- list(lgpr:::average_predictions(PRED1$LIST))
#PRED2   <- PREDS[[2]]
#PRED2$LIST <- list(lgpr:::average_predictions(PRED2$LIST))


edit_plot <- function(gg_list){
  col1 <- bayesplot::color_scheme_get("brightblue")$dark
  col2 <- bayesplot::color_scheme_get("red")$dark
  values <- c(col1, col2)
  values2 <- c("steelblue2", "firebrick3")
  gg_list[[3]] <- gg_list[[3]] + scale_color_manual(values = values)
  gg_list[[3]] <- gg_list[[3]] + scale_fill_manual(values = values)
  gg_list[[4]] <- gg_list[[4]] + scale_color_manual(values = values)
  gg_list[[4]] <- gg_list[[4]] + scale_fill_manual(values = values)
  gg_list[[5]] <- gg_list[[5]] + scale_color_manual(values = values)
  gg_list[[5]] <- gg_list[[5]] + scale_fill_manual(values = values) 
  gg_list[[6]] <- gg_list[[6]] + scale_color_manual(values = values)
  gg_list[[6]] <- gg_list[[6]] + scale_fill_manual(values = values2)
  for(j in 1:6){
    h <- gg_list[[j]]
    h <- h + theme(panel.grid.minor = element_blank()) + 
      theme(legend.title=element_blank(), 
            legend.text=element_text(size=7))
    h <- h + theme(legend.position = c(0.7, 0.2))
    gg_list[[j]] <- h
  }
  return(gg_list)
}

color1 <- bayesplot::color_scheme_get("brightblue")$dark
color2 <- "firebrick3"

gg1 <- plot_components_posterior(fit1, PRED = PRED1, 
                                 return_list = TRUE, 
                                 fill_alpha = 0.15,
                                 linealpha = 1,
                                 sum_highlight = "group")
gg2 <- plot_components_posterior(fit2, PRED = PRED2, 
                                 return_list = TRUE, 
                                 fill_alpha = 0.15,
                                 linealpha = 1,
                                 sum_highlight = "group")
gg1 <- edit_plot(gg1)
gg2 <- edit_plot(gg2)

dat       <- fit1@model@data
dat$y     <- fit1@model@scalings$YSCL$fun(dat$y)
dat$id    <- as.factor(dat$id)
dat$sex   <- as.factor(dat$sex)
dat$group <- as.factor(dat$group)
aff_id    <- as.numeric(names(which(affected(fit1))))
isaff     <- dat$id %in% aff_id
dat$isaff <- as.factor(isaff)

PSIZE <- 1
dp1 <- plot_data(dat, highlight = "disease", psize = PSIZE) + 
  theme_bw() + ggtitle(" ") +
  theme(legend.position = c(0.3,0.3),
        legend.title = element_blank()) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()) +
  scale_color_manual(values = c(color2, color1)) +
  ylab(prot_name) + xlab('Age (months)')
dp2 <- plot_data(dat, highlight = "isaff", psize = PSIZE) + 
  theme_bw() + ggtitle(" ") +
  theme(legend.position = c(0.3,0.3)) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()) +
        scale_color_manual(values = c("gray50", color2)) +
  ylab(prot_name) + xlab('Age (months)')

r2aa <- bplots(fit1)
r2bb <- bplots(fit2)

row2b <- ggarrange(gg1[[1]], gg1[[2]], gg1[[3]],
                   gg1[[4]], gg1[[5]], gg1[[6]], 
                   r2aa[[2]], r2aa[[1]],
                   nrow = 1, ncol = 8)

row2a <- ggarrange(gg2[[1]], gg2[[2]], gg2[[3]],
                   gg2[[4]], gg2[[5]], gg2[[6]], 
                   r2bb[[2]], r2bb[[1]],
                   nrow = 1, ncol = 8)

row3a <- ggarrange(dp1, dp2, nrow = 1, ncol = 2)


bp <- plot_beta(fit1) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())
row3b <- bp + facet_wrap(. ~ Parameter, scales = "free", nrow = 2) +
  theme(strip.background = element_blank(),
         strip.text.x = element_blank())

row3 <- ggarrange(row3a, row3b, dp1, nrow = 1, ncol = 3, widths = c(1,1.5,0.7))
full <- ggarrange(row1, row2a, row2b, row3,
                  nrow = 4, ncol = 1,
                  heights = c(1,0.95,0.95,1))



#theme(legend.position = c(0.6,0.4), 
##      legend.text=element_text(size=rel(0.75)), legend.title=element_blank()) +
#  scale_colour_brewer(type = "qual", palette = "Set1", direction = 1,
#                      aesthetics = "colour")
# n_smp   <- 4*1/2*fit1@stan_fit@stan_args[[1]]$iter
# i_smp   <- sample.int(n_smp, 100, replace = FALSE)

h1 <- plot_posterior_f(fit1, PRED1, original_y_scale = F)
h2 <- plot_posterior_f(fit2, PRED2, original_y_scale = F)
