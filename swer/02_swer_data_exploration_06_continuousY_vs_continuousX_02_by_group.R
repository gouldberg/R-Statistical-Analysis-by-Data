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
# data exploration:  Y vs. continuous X  by ggplot by group
# ------------------------------------------------------------------------------


# scale_y_log10(): plot the response and all other features on a log scale

library(ggplot2)

# gg <- ggplot(swer, aes(exra^(-0.65), nao)) + 
gg <- ggplot(swer, aes(exra^(-0.65), year)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "exra", x = "nao")


gg + facet_wrap(~ climate.region)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# ------------------------------------------------------------------------------

formula = exra ~ nao | climate.region

coplot(formula, data = swer, ylab = "exra", xlab = "nao", las=1)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# ------------------------------------------------------------------------------

MyXLab <- "year"

MyYLab <- "exra"

par(mfrow=c(1,1), mar = c(5,5,2,3))

plot(x = swer$year, y = swer$exra, xlab = MyXLab, ylab = MyYLab, col= swer$climate.region, cex.lab = 1.5)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# ------------------------------------------------------------------------------

xyplot(exra ~ year | climate.region, data = swer, cex = 0.3, col = "black", type = c("p", "g", "smooth"))


# -->
# clearly space-time interaction is required
