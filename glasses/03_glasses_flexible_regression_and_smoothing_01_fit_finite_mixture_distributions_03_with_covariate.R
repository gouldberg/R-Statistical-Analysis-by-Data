setwd("//media//kswada//MyFiles//R//glasses")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glasses
# ------------------------------------------------------------------------------
data("glasses", package = "gamlss.data")


str(glasses)

car::some(glasses)



# ------------------------------------------------------------------------------
# Fit two-component distribution models, using sex as a covariate
# ------------------------------------------------------------------------------

library(gamlss.mx)

set.seed(3683)



# ----------
readLO1 <- gamlssMX(ageread ~ sex, family = LO, K = 2, data = glasses)

readLO2 <- gamlssMX(ageread ~ sex, pi.formula = ~ sex, family = LO, K = 2, data = glasses)

readLO3 <- gamlssMX(ageread ~ 1, pi.formula = ~ sex, family = LO, K = 2, data = glasses)



# ----------
AIC(readLO, readLO1, readLO2, readLO3)


# -->
# The preferred model is readLO2, which has sex as a covariate for both mu and pi


# ----------
readLO2



# ------------------------------------------------------------------------------
# plot distributions
# ------------------------------------------------------------------------------
fnloFemale <- getpdfMX(readLO2, observation = 1)
fnloMale <- getpdfMX(readLO2, observation = 2)

par(mfrow=c(1,2))
truehist(glasses$ageread[glasses$sex == 1], nbins = 25, col = "grey", xlab = "Age", ylab = "Density", ymax = 0.05, main = "Males")
lines(seq(0.5, 90.5, 1), fnloMale(seq(0.5, 90.5, 1)), lwd = 2)

truehist(glasses$ageread[glasses$sex == 2], nbins = 25, col = "grey", xlab = "Age", ylab = "Density", ymax = 0.05, main = "Females")
lines(seq(0.5, 90.5, 1), fnloMale(seq(0.5, 90.5, 1)), lwd = 2)



