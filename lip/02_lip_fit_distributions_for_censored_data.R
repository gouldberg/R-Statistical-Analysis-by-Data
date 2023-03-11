setwd("//media//kswada//MyFiles//R//lip")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lip
# ------------------------------------------------------------------------------
data("lip", package = "gamlss.cens")


str(lip)

car::some(lip)



# ------------------------------------------------------------------------------
# Extend GAMLSS family distribution to generate an interval-censored Weibull distribution
# ------------------------------------------------------------------------------

library(gamlss.cens)


# ----------
# An interval-censored Weibull, WEI2(mu, sigma), distribution is now generated which allows interval- (as well as left- and right-) censored
# response values
gen.cens(WEI2, type = "interval")


WEI2ic()



# ------------------------------------------------------------------------------
# Fit by WEI2ic distribution
# ------------------------------------------------------------------------------

weimi <- gamlss(y ~ poly(Tem, 2) + poly(pH, 2) + poly(aw, 2), data = lip, family = WEI2ic, n.cyc = 100, trace = TRUE)


summary(weimi)


plot(weimi)