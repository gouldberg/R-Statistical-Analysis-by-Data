setwd("//media//kswada//MyFiles//R//eu15")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EU15
# ------------------------------------------------------------------------------
data("eu15", package = "gamlss.data")


str(eu15)

car::some(eu15)



# ----------
eu15 <- transform(eu15, lGDP = log(GDP), lCapital = log(Capital), lLabor = log(Labor), lUsefulEnergy = log(UsefulEnergy))

car::some(eu15)



# ------------------------------------------------------------------------------
# Fit simple linear GAMLSS model using the TF2 distribution  (t Family repar)
# ------------------------------------------------------------------------------

w <- 1 * exp(-(2 / (nrow(eu15))) * seq(from = nrow(eu15) - 1, to = 0))

mod2 <- gamlss(lGDP ~ lCapital + lUsefulEnergy + lLabor, data = eu15, family = TF2, weights = w)



# ----------
par(mfrow = c(1, 1))
with(eu15, plot(Year, lGDP, pch = 21))
lines(fitted(mod1)[order(eu15$Year)] ~ eu15$Year[order(eu15$Year)], lwd = 1)
lines(fitted(mod2)[order(eu15$Year)] ~ eu15$Year[order(eu15$Year)], lwd = 2, col = "blue")



# ----------
# Generalized R^2 is better for mod1
Rsq(mod1)
Rsq(mod2)



# ----------
# generalized AIC is better for mod1
GAIC(mod1, mod2)

