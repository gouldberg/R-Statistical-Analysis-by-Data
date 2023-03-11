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
# Model Comparison by Likelihood Ratio Test
# ------------------------------------------------------------------------------

# We can caompre a given Poisson model to its negative-binomial counterpart, where are nested

lmtest::lrtest(modp, mod.nbin)



# -->
# Negative Binomial model is significantly better



# ----------
lmtest::lrtest(mod.nbin, mod.nbin2)



# -->
# But Negative Binomial Model with interactions included are NOT significantly better



# ----------
lmtest::lrtest(mod.zpois, mod.znbin)


lmtest::lrtest(mod.hpois, mod.hnbin)


# lmtest::lrtest(mod.znbin, mod.hnbin)


# -->
# For both of hurdle model and Zero-Inflated model, binomial negative model is significantly better



# ------------------------------------------------------------------------------
# Model Comparison for non-nested model
# ------------------------------------------------------------------------------

library(pscl)


# Of greater interest is the difference among the negative binomial models that are not nested.


# nbin vs. hurdle nbin  (nbin SHOULD BE glm.nb class)
vuong(nbin, mod.hnbin)



# -->
# NB model is considered to be a better fit than the hurdle version (because more parsimonious)



# ----------
# hurdle nbin  vs. hurdle nbin
vuong(mod.hnbin, mod.znbin)



# -->
# model2 (zero-inflated NB model) is significantly better fit than hurdle NB model (by BIC-corrected version) 



# ------------------------------------------------------------------------------
# Compare AIC and BIC
# ------------------------------------------------------------------------------

LRstats(modp, mod.nbin, mod.nbin2, mod.zpois, mod.znbin, mod.hpois, mod.hnbin, sortby = "BIC")


# -->
# AIC:  mod.znbin is the best
# BIC:  mod.nbin is the best





