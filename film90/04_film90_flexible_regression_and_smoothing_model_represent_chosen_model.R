setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss", "gamlss.add")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# Fitted smooth functions for parameters
# ------------------------------------------------------------------------------

# fitted smooth functions for mu, sigma, nu, and tau for models m5 and m6
# m5 has only 3 parameters so does not appear in bottom right panel.
fittedPlot(m5, m6, x = film90$lboopen, line.type = TRUE)



# ------------------------------------------------------------------------------
# centile estimates forr the fitted distritbution
# ------------------------------------------------------------------------------

# Since, in this example, only one explanatory variable is used in the fit, centile estimates for the fitted distribution can be shown
# THe lowest curve is the fitted 3% centile curve, defined by 3% of the values of lborev1 lying belowe the curve for each value of lboopen,
# for the fitted model m6 if it was the correct model.
centiles.fan(m6, xvar = film90$lboopen, cent = c(3, 10, 25, 50, 75, 90, 97),
             colors = "terrain", ylab = "lborev1", xlab = "lboopen")



# ------------------------------------------------------------------------------
# Fitted conditional distribution
# ------------------------------------------------------------------------------
library(gamlss.util)
library(colorspace)

plotSimpleGamlss(lborev1, lboopen, model = m6, data = film90, x.val = seq(6, 16, 2), val = 5, N = 1000, ylim = c(0,25), cols = heat_hcl(100))

