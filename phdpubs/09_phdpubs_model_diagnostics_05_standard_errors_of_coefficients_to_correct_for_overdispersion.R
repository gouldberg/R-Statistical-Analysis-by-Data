setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ------------------------------------------------------------------------------
# Compare the standard errors of coeffs for the various models
# ------------------------------------------------------------------------------

# This is the version of SE to be corrected for overdispersion

library(sandwich)


SE <- sqrt(cbind(
  pois = diag(vcov(modp)),
  sand = diag(sandwich(modp)),
  qpois = diag(vcov(mod.qpois)),
  nbin = diag(vcov(mod.nbin)),
  nbin2 = diag(vcov(mod.nbin2)),
  hpois = diag(vcov(mod.hpois)),
  hnbin = diag(vcov(mod.hnbin)),
  zpois = diag(vcov(mod.zpois)),
  znbin = diag(vcov(mod.znbin))
))


round(SE, digits=4)


# ----------
psych::describe(SE)

