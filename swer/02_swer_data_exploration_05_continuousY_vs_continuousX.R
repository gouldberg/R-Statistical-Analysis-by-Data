setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(exra ~ nao, data = swer, ylab = "exra", cex.lab = 1.25, xlab = "nao", pch = 20, col = gray(0.7))
lines(lowess(swer$nao, swer$exra), col = "blue", lwd = 1)


plot(exra ~ elevation, data = swer, ylab = "exra", cex.lab = 1.25, xlab = "elevation", pch = 20, col = gray(0.7))
lines(lowess(swer$elevation, swer$exra), col = "blue", lwd = 1)


plot(exra ~ year, data = swer, ylab = "exra", cex.lab = 1.25, xlab = "year", pch = 20, col = gray(0.7))
lines(lowess(swer$year, swer$exra), col = "blue", lwd = 1)


# -->
# something strange that as elevation increases, exra is increasing ...



# ----------
# after transformation
plot(exra^(-0.65) ~ nao, data = swer, ylab = "exra", cex.lab = 1.25, xlab = "nao", pch = 20, col = gray(0.7))
lines(lowess(swer$nao, (swer$exra)^(-0.65)), col = "blue", lwd = 1)


plot(exra^(-0.65) ~ elevation, data = swer, ylab = "exra", cex.lab = 1.25, xlab = "elevation", pch = 20, col = gray(0.7))
lines(lowess(swer$elevation, (swer$exra)^(-0.65)), col = "blue", lwd = 1)


plot(exra^(-0.65) ~ year, data = swer, ylab = "exra", cex.lab = 1.25, xlab = "year", pch = 20, col = gray(0.7))
lines(lowess(swer$year, (swer$exra)^(-0.65)), col = "blue", lwd = 1)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications

library(ggplot2)

gg <- ggplot(swer, aes(exra^(-0.65), nao)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "exra", x = "nao")


gg <- ggplot(swer, aes(exra^(-0.65), year)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "exra", x = "year")


gg



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles
par(mfrow = c(1,1))

plot(exra ~ cut(elevation, 10), data = swer, ylab = "exra", xlab = "nao", las=1)

