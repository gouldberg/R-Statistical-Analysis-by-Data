setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "lmtest", "ggplot2", "directlabels", "effects", "car", "nnet")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)



# ------------------------------------------------------------------------------
# Convert 3-way table to frequency form
# ------------------------------------------------------------------------------

TV.df <- as.data.frame.table(TV)

car::some(TV.df)



# ----------
levels(TV.df$Network)


# choose "ABC" clearly as baseline category
TV.df$Network <- relevel(TV.df$Network, ref = "ABC")

levels(TV.df$Network)



# ------------------------------------------------------------------------------
# Fitted probabilities
# ------------------------------------------------------------------------------

predictors <- expand.grid(Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
                          Time = c("8:00", "8:15", "8:30", "8:45", "9:00", "9:15", "9:30", "9:45", "10:00", "10:15", "10:30"))

( fit <- data.frame(predictors, predict(tv.multinom, predictors, type = "probs")) )


fit2 <- reshape2::melt(fit,
                       measure.vars = c("ABC", "CBS", "NBC"),
                       variable.name = "Network",
                       value.name = "Probability")

levels(fit2$Network) <- c("ABC", "CBS", "NBC")

fit2



# ----------
library(ggplot2)


# by Day and Network
gg <- ggplot(fit2, aes(x = Day, y = Probability)) + facet_grid(~ Network) + geom_boxplot(outlier.shape = NA) + geom_jitter(size = 0.2) + theme_bw() 

gg



# ----------
# by Time and Network
gg <- ggplot(fit2, aes(x = Time, y = Probability)) + facet_grid(~ Network) + geom_boxplot(outlier.shape = NA) + geom_jitter(size = 0.2) + theme_bw() 

gg




