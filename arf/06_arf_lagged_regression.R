setwd("//media//kswada//MyFiles//R//arf")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  arf
# ------------------------------------------------------------------------------

data(arf, package = "astsa")

str(arf)

head(arf)



# ------------------------------------------------------------------------------
# Regression with lagged variables with align lagged series
# ------------------------------------------------------------------------------

# align the lagged series
( arf_2 <- ts.intersect(arf, arfdiff = diff(arf), arf1 = stats::lag(diff(arf), -1), arf13 = stats::lag(diff(arf), -13), dframe = TRUE) )



# ----------
# Regress on only lag(soi, -6)
fit1 <- lm(dffarf ~ arf1 + arf13, data = arf_2, na.action = NULL)


summary(fit1)


par(mfrow = c(2,2))
plot(fit1)



# -->
# All terms are significant



# ------------------------------------------------------------------------------
# Regression with lagged variables:  no need to align lagged series
# ------------------------------------------------------------------------------

# The headache of aligning the lagged series can be avoided by using dynlm
library(dynlm)


# dynlm model have time series attributes without any additional commands
fit2 <- dynlm(diffarf ~ L(arf, 1) + L(arf, 13))


summary(fit2)



# ----------
plot(fit2)
