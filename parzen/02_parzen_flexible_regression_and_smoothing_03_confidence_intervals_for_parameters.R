setwd("//media//kswada//MyFiles//R//parzen")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: parzen
#   - Variables:
#        - snowfall:  the annual snowfall in Buffalo, NY (inches) from 1910 to 1972 inclusive
# ------------------------------------------------------------------------------

data("parzen", package = "gamlss.data")


str(parzen)

car::some(parzen)



# ------------------------------------------------------------------------------
# Check standard errors for the fitted parameters
# ------------------------------------------------------------------------------

m1 <- gamlss(snowfall ~ 1, data = parzen, family = WEI3, trace = FALSE)



# ----------
# default summary() fives the SE obtained by vcov.
# standard errors obtained by vcov() are the ones obtained by inverting the full Hessian matrix
# and they do take into account the correlations between the distibution parameter estimates.

summary(m1)


vcov(m1, type = "se")


# ------------------------------------------------------------------------------
# Approximated confidence interval
# ------------------------------------------------------------------------------

sgm <- m1$sigma.coefficients


# approximate 95% CI confidence interval for sigma
exp(sgm - 1.96 * vcov(m1, type="se")[2])
exp(sgm + 1.96 * vcov(m1, type="se")[2])



# ----------
# more reliable profile deviance 95% CI
prof.dev(m1, "sigma", min=3, max=4.8, step=0.01)



# ------------------------------------------------------------------------------
# bootstrap 95% CI for sigma
# ------------------------------------------------------------------------------

library(boot)

set.seed(1453)

mod1 <- gamlss(snowfall ~ 1, data = parzen, family = WEI3, trace = FALSE)

funB <- function(data, i){
  d <- data.frame(snowfall = data[i,])
  coef(update(mod1, data = d), "sigma")
}


( mod1.boot <- boot(parzen, funB, R=199, parallel="multicore", ncpus = 4) )


# ----------
boot.ci(mod1.boot, type = c("norm", "basic"))


# -->
# requires exp()







