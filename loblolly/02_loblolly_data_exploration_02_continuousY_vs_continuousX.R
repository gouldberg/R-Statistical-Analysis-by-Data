setwd("//media//kswada//MyFiles//R//loblolloy")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 2. GAMs in Practice: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Loblloly
# ------------------------------------------------------------------------------

data(Loblolly, package = "gamair")


# Note that this is "nfnGroupedData" class
str(Loblolly)


head(Loblolly)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X
# ------------------------------------------------------------------------------

plot(height ~ age, data = Loblolly, ylab = "height", xlab = "age", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Loblolly$age, Loblolly$height), col = "blue", lwd = 1)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

xyplot(height ~ age, data = Loblolly)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(Loblolly, aes(x = age, y = height)) + xlab("age") + ylab("height") + geom_point(position = position_jitter(), alpha = 0.3) + stat_smooth()



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by scatterplot
# ------------------------------------------------------------------------------

formula <- height ~ age

car::scatterplot(formula, data = Loblolly)


