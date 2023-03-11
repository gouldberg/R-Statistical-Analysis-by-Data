setwd("//media//kswada//MyFiles//R//cairo")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cairo
#   - daily temperature in Cairo over nearly a decade.
#     The data are from:   http:://www.engr.udayton.edu/weather/citylistWorld.htm
# ------------------------------------------------------------------------------

data("cairo", package = "gamair")

str(cairo)

head(cairo)



# ----------
car::some(cairo)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(2, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

tmp <- astsa::acf2(diff(cairo$temp), 1000)

head(tmp)


# -->
# ACF and PACF does not taling off
# but very strong correlation at lag 2