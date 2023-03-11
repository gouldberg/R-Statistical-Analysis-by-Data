setwd("//media//kswada//MyFiles//R//mack")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mack
# ------------------------------------------------------------------------------

data(mack, package = "gamair")

data(mackp, package = "gamair")

data(coast, package = "gamair")


str(mack)

str(mackp)

str(coast)



# ----------
# mackp contains prediction grid data for 1992 mackerel egg model.
# A data from with 5 columns. Each row corresponds to one spatial location within the survey area.

# coast:  European coastline from -11 to 0 East and from 43 to 59 North



# ------------------------------------------------------------------------------
# Simple Poisson and quasipoisson model
# ------------------------------------------------------------------------------

mack$log.net.area <- log(mack$net.area)


mod_glm <- glm(egg.count ~ I(b.depth^.5) + c.dist + salinity + temp.surf + temp.20m + offset(log.net.area),
            data = mack, family = poisson)



mod_glm2 <- glm(egg.count ~ I(b.depth^.5) + c.dist + salinity + temp.surf + temp.20m + offset(log.net.area),
               data = mack, family = quasipoisson)



# ----------
summary(mod_glm)


summary(mod_glm2)



# -->
# mod_glm2:  Dispersion parameter for quasipoisson family taken to be 15.6



# ------------------------------------------------------------------------------
# model diagnostics
# ------------------------------------------------------------------------------

car::residualPlots(mod_glm)


car::residualPlots(mod_glm2)



graphics.off()
par(mfrow=c(1,1))
plot(mod_glm)



graphics.off()
par(mfrow=c(1,1))
plot(mod_glm2)



# -->
# very strong evidence of over-dispersion: the residuals are much too large for consistency with a Poisson with unit scale parameter.
# An alternative is to use the quasipoisson family in R, which makes the Poisson-like assumption that var(yi) = phi * E(yi), but without fixing phi = 1.
# However, to facilitate things like AIC model seletion and REML smoothing parameter estimation
# it helps to use full distributions.
# The obvious alternatives are the Tweedie or negative binomial distribution.
