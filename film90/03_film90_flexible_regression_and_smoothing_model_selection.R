setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss", "gamlss.add")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# Selection between models
#   - gamlss() global deviance is different from the deviance provided by glm() and gam().
#     The gloabl deviance is EXACTLY minus twice the fitted log-likelihood function, including all constant terms in the log-likelihood.
#     glm() deviance is calculated as a deviation from the saturated model. It does not include constant terms, and so cannot be used to compare differnt distritbutions.
# ------------------------------------------------------------------------------

# compare models
# GAIC() uses default penalty k = 2, resultng in the AIC.
GAIC(m0, m1, m2, m3, mnt, m4, m5, m6)



# ----------
# SBC (k = log(n))
GAIC(m0, m1, m2, m3, mnt, m4, m5, m6, k = log(4031))


