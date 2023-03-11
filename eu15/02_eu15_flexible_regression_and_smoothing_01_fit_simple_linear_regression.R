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

mod1 <- gamlss(lGDP ~ lCapital + lUsefulEnergy + lLabor, data = eu15, family = TF2)

summary(mod1)



# ----------
# Plot regression terms for a specified parameter of a fitted GAMLSS object
term.plot(mod1, pages = 1)



# ----------
with(eu15, plot(Year, lGDP, pch = 21))

lines(fitted(mod1)[order(eu15$Year)] ~ eu15$Year[order(eu15$Year)], lwd = 2)

