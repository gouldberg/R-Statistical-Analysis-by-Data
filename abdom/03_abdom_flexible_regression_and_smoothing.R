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
# Flexible regression and smoothing
# ------------------------------------------------------------------------------

# family = BCT:  The Box-Cox t distribution

abd10 <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = BCT, trace = FALSE)


summary(abd10)


