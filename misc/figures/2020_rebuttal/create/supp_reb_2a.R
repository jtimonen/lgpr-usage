library(lgpr) # requires lgpr 0.33.3
library(ggplot2)
require(ggpubr)

source('generate_reb2.R')

gen <- generate_data(1, 16, by = 0.5)
plots <- list()

j <- 1
id <- gen$data$id
age <- gen$data$age

# (1) id x age
f <- gen$comp[1,]
df <- data.frame(id, age, f)
aesth <- aes(x = age, y = f, group = id)
plots[[1]] <- ggplot(df, aesth) + geom_line()

# (2) age
f <- gen$comp[2,]
df <- data.frame(id, age, f)
aesth <- aes(x = age, y = f)
plots[[2]] <- ggplot(df, aesth) + geom_line()

# (3) disease-related age
grp <- as.factor(is.nan(gen$data$diseaseAge))
f <- gen$comp[3,]
df <- data.frame(id, age, f, grp)
aesth <- aes(x = age, y = f, group = id, color = grp)
plots[[3]] <- ggplot(df, aesth) + geom_line() + theme(legend.position = "none")

# (4) x1
f <- gen$comp[4,]
x <- gen$data$x1
df <- data.frame(id, x, f)
aesth <- aes(x = x, y = f)
plots[[4]] <- ggplot(df, aesth) + geom_line()

# (5) x2
f <- gen$comp[5,]
x <- gen$data$x2
df <- data.frame(id, x, f)
aesth <- aes(x = x, y = f)
plots[[5]] <- ggplot(df, aesth) + geom_line()

# (6) z1 x age
f <- gen$comp[6,]
z <- gen$data$z1
df <- data.frame(id, age, f, as.factor(z))
colnames(df)[4] <- "z"
aesth <- aes(x = age, y = f, group = z, color = z)
plots[[6]] <- ggplot(df, aesth) + geom_line() + theme(legend.position = "none")

# (7) z2 x age
f <- gen$comp[7,]
z <- gen$data$z2
df <- data.frame(id, age, f, as.factor(z))
colnames(df)[4] <- "z"
aesth <- aes(x = age, y = f, group = z, color = z)
plots[[7]] <- ggplot(df, aesth) + geom_line() + theme(legend.position = "none")

# full plot
a <- ggarrange(plotlist = plots)
