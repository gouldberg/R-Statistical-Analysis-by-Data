# setwd("//media//kswada//MyFiles//R//pilotshop_sales//")
setwd("//media//kswada//MyFiles//R//Bayesian_statistics//pilotshop_sales//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


# March Osaka data
Osaka_March <- c(data$y[,1], data$x)


summary(Osaka_March)



# ----------
# May Tokyo data
Tokyo_May <- c(data$y[,2])


summary(Tokyo_May)



# ----------
graphics.off()

par(mfrow = c(2,1))

hist(Osaka_March, breaks = seq(0, 160, by = 5))

hist(Tokyo_May, breaks = seq(0, 160, by = 5))



# ----------
car::scatterplot(y[,1] ~ y[,2])


psych::pairs.panels(y)


# -->
# correlation is only 0.60



# -->
# Correlation is 0.83
