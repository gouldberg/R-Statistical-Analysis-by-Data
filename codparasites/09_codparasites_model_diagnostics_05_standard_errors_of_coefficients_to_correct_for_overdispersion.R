setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



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
  hpois = diag(vcov(mod.hpois)),
  hnbin = diag(vcov(mod.hnbin)),
  zpois = diag(vcov(mod.zpois)),
  znbin = diag(vcov(mod.znbin))
))


round(SE, digits=4)


# ----------
# Note that Hurdle NB model (here the best) is lowest absolute value in skew and kurtosis of SE distribution of all variables
psych::describe(SE)

