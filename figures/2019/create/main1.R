library(lgpr)
library(bayesplot)
library(ggplot2)
library(ggpubr)
require(reshape2)
require(viridisLite)

# ROW 1 -------------------------------------------------------------------


# Create MCMC samples 3d illustration
f <- lgpr:::create_example_fit(N = 6, iter = 1000, chains = 3)  
df <- as.data.frame(f@stan_fit)
p1 <- df$`alpha_idAge[1]`
p2 <- df$`ell_idAge[1]`
p3 <- df$`sigma_n[1]`
lp <- df$lp__
DF <- data.frame(p1, p2, lp)
plot_mcmc <- ggplot(DF, aes(x=p1,y=p2,color=lp)) + geom_point() +
  scale_colour_viridis_c() + theme_classic() + 
  xlab(expression(theta[1])) + ylab(expression(theta[2])) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle('MCMC sampling')

# Create user input frame
input <- ggplot() + theme_void()

# Create relevance determination frame
relev <- ggplot() + theme_void()

# Create visualization frame
vis <- ggplot() + theme_void()

row1 <- ggarrange(input, plot_mcmc, 
                  relev, vis, nrow = 1, ncol = 5)


# ROW 2 -------------------------------------------------------------------


set.seed(123211)
simData <- simulate_data(N = 8, t_data = seq(0.5, 60, 0.5),
                         covariates   = c(0,1,2),
                         n_categs     = c(2),
                         names        = c("diseaseAge", "bloodPressure", "sex"),
                         relevances   = c(1,1,1,1,1), 
                         lengthscales = c(12,24,1,1,18), 
                         steepness    = 0.5,
                         t_effect_range = c(20,40),
                         vm_params    = c(0.025, 1),
                         force_zeromean = TRUE,
                         verbose      = TRUE)

row2a <- plot_components_simdata(simData, 
                                 marker = NA, 
                                 nrow = 2, ncol = 3, 
                                 sum_highlight = "group")


row2b <- plot_components_simdata(simData, 
                                 marker = NA, 
                                 nrow   = 2, ncol = 3, 
                                 sum_highlight = "group",
                                 time_is_xvar = FALSE)

row2 <- ggarrange(row2a, row2b, ncol = 2, nrow = 1, labels = c("b", "c"))


dat            <- simData$data
sex            <- as.factor(dat$sex)
age            <- dat$age
bloodPressure  <- dat$bloodPressure
diseaseAge     <- dat$diseaseAge
id             <- as.factor(dat$id)
group          <- as.factor(as.numeric(!is.nan(diseaseAge)))
DF  <- data.frame(id, age, bloodPressure, diseaseAge)
row3a <- ggplot(DF, aes(x = age, y = bloodPressure, group = id)) + geom_line() +
  theme_linedraw() + ggtitle(" ")
row3b <- ggplot(DF, aes(x = age, y = diseaseAge, group = id)) + geom_line() +
  theme_linedraw() + ggtitle(" ")

row3 <- ggarrange(row3a, row3b, nrow = 1, ncol = 4) 

# FULL PLOT ---------------------------------------------------------------

full <- ggarrange(row1, row2, row3, nrow = 3, ncol = 1, 
                  labels = c("a", " ", " ", "d"),
                  heights = c(1, 2.25, 1.35))


# MODIFIED

row2a <- plot_components_simdata(simData, 
                                 marker = NA, 
                                 nrow = 1, ncol = 6, 
                                 sum_highlight = "group",
                                 return_list = TRUE,
                                 font_size = 10)

new <- simData$components[c("age", "sex.age", "diseaseAge", "bloodPressure", "id.age")]
colnames(new) <- c("f1", "f12", "f123", "f1234", "f12345")
new[,2] <- new[,1] + new[,2]
new[,3] <- new[,2] + new[,3]
new[,4] <- new[,3] + new[,4]
new[,5] <- new[,4] + new[,5]
id      <- simData$data$id
age     <- simData$data$age

ymin  <- 4.5
ymax  <- 6
fcum  <- new$f1
df1   <- data.frame(id,age,sex, fcum)
pcum1 <- ggplot(df1, aes(x=age,y=fcum,group=sex)) + geom_line()
pcum1 <- pcum1 + theme_bw() + theme(legend.position = c(0.4,0.4)) +
  theme(panel.grid = element_blank()) + ylim(-ymin,ymax) + 
  theme(axis.title.y = element_blank())

fcum   <- new$f12
df12   <- data.frame(id,age,sex, fcum)
pcum12 <- ggplot(df12, aes(x=age,y=fcum,group=sex,color=sex)) + geom_line()
pcum12 <- pcum12 + theme_bw() + theme(legend.position = c(0.4,0.4)) +
  theme(panel.grid = element_blank()) + ylim(-ymin,ymax) + 
  theme(axis.title.y = element_blank())

fcum    <- new$f123
df123   <- data.frame(id,age,group, fcum)
pcum123 <- ggplot(df123, aes(x=age,y=fcum,group=id,color=group)) + geom_line()
pcum123 <- pcum123 + theme_bw() + theme(legend.position = c(0.4,0.4)) +
  theme(panel.grid = element_blank()) + ylim(-ymin,ymax) + 
  theme(axis.title.y = element_blank())

fcum     <- new$f1234
df1234   <- data.frame(id,age,group,bloodPressure, fcum)
pcum1234 <- ggplot(df1234, aes(x=age,y=fcum,group=id,color=bloodPressure)) + geom_line()
pcum1234 <- pcum1234 + theme_bw() + theme(legend.position = c(0.4,0.4))  +
  theme(panel.grid = element_blank()) + scale_color_viridis_c()  + ylim(-ymin,ymax)+
  theme(axis.title.y = element_blank())

fcum      <- new$f12345
df12345   <- data.frame(id,age,group, fcum)
pcum12345 <- ggplot(df12345, aes(x=age,y=fcum,group=id,color=group)) + geom_line()
pcum12345 <- pcum12345 + theme_bw() + theme(legend.position = c(0.4,0.4)) +
  theme(panel.grid = element_blank()) + ylim(-ymin,ymax) +
  theme(axis.title.y = element_blank())

pnew <- ggarrange(row2a[[2]], row2a[[5]], row2a[[3]], row2a[[4]], row2a[[1]],
                  pcum1, pcum12, pcum123, pcum1234, pcum12345,
                  nrow = 2, ncol = 5) + ylim(-ymin,ymax)
