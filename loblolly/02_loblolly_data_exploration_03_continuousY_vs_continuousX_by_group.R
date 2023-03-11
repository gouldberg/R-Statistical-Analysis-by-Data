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
# data exploration:  continuous Y vs. continuous X  by group   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(Loblolly, aes(age, height)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "height", x = "age")


gg + facet_wrap(~ Seed)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by coplot
# ------------------------------------------------------------------------------

formula = height ~ age | Seed

coplot(formula, data = Loblolly, ylab = "height", xlab = "age", las=1)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

formula = height ~ age | Seed

xyplot(formula, data = Loblolly, type = c("p", "g", "smooth"))




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by scatterplot
# ------------------------------------------------------------------------------

formula <- height ~ age | Seed

car::scatterplot(formula, data = Loblolly)



