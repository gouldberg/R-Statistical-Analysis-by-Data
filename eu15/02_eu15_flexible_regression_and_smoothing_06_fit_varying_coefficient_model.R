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
# Fit varying coefficient model whereby the coefficients of capital, useful energy and labor change smoothly according to year
# ------------------------------------------------------------------------------


mod.pvc <- gamlss(lGDP ~ pvc(Year, by = lCapital) + pvc(Year, by = lUsefulEnergy) + pvc(Year, by = lLabor), dat = eu15, family = TF2, n.cyc = 100)



# ----------
term.plot(mod.pvc, rug = T, ylim = "free", ask = F, scheme = "lines", pages = 1)



# ----------
# varying coefficient model is the best
GAIC(mod1, mod3, mod4, mod5, mod.pvc)