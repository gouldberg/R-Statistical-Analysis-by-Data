setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# Compare the standard errors of coeffs for the various models
# ------------------------------------------------------------------------------

# This is the version of SE to be corrected for overdispersion

library(sandwich)


SE <- sqrt(cbind(
  pois = diag(vcov(modp2)),
  sand = diag(sandwich(modp2)),
  qpois = diag(vcov(mod.qpois)),
  nbin = diag(vcov(mod.nbin))
))


round(SE, digits=4)



# ----------
psych::describe(SE)

