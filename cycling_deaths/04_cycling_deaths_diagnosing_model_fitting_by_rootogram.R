setwd("//media//kswada//MyFiles//R//cycling_deaths")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  London Cycling Deaths data
# ------------------------------------------------------------------------------
data("CyclingDeaths", package = "vcdExtra")

data <- CyclingDeaths

data

( tab <- table(data$deaths) )



# ------------------------------------------------------------------------------
# Fit the poisson distribution
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "poisson")



# ------------------------------------------------------------------------------
# Plot for data and fitting Poisson model
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------
plot(data_fit, type = "hanging", shade = TRUE, xlab = "Number of deaths")

plot(data_fit, type = "deviation", shade = TRUE, xlab = "Number of deaths")

