setwd("//media//kswada//MyFiles//R//star")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  star
# ------------------------------------------------------------------------------

data(star, package = "astsa")


str(star)

head(star)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial-autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))

astsa::acf2(star, max.lag = 100)



# -->
# Autocorrelation function is cyclic and NOT tailing off ....