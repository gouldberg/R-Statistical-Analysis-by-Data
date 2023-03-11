setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# Nonparametric bootstrap to calculate confidence intervals for smoothing parameter
# ------------------------------------------------------------------------------

library(boot)


fitG <- gamlss(y ~ pb(x), sigma.fo = ~x, data = abdom, family = LO, gd.tol = Inf)



# ----------
abd.Smo <- function(data, i){
  d <- data[i,]
  getSmo(update(fitG, data=d))$lambda
}


abd.Sim <- boot(abdom, abd.Smo, R = 99)



# ----------
plot(abd.Sim, index = 1)

boot.ci(abd.Sim, type = "perc")



