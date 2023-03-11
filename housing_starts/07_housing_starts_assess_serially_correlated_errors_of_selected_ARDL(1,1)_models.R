# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/housing_starts")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# --> continued from previous scripts

# ------------------------------------------------------------------------------
# Assess serially correlated residuals of regM1, regM4, regM7
#  - plot residuals
# ------------------------------------------------------------------------------
graphics.off()
par(mfrow = c(3,1))

plot(residuals(regM1))
lines(lowess(residuals(regM1)), col="blue")

plot(residuals(regM4))
lines(lowess(residuals(regM4)), col="blue")

plot(residuals(regM7), type = "l")
lines(lowess(residuals(regM7)), col="blue")


# -->
# Note that all those 3 models have almost same residuals and predictions


# ------------------------------------------------------------------------------
# Assess serially correlated residuals of regM1, regM4, regM7
#  - residual correlogram
# ------------------------------------------------------------------------------
graphics.off()
par(mfrow = c(3,1))

acf(residuals(regM1), main = "")

acf(residuals(regM4), main = "")

acf(residuals(regM7), main = "")


# -->
# Shows no serial correlation

