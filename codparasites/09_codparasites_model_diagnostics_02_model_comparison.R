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
# Model Comparison by Likelihood Ratio Test
# ------------------------------------------------------------------------------

# We can caompre a given Poisson model to its negative-binomial counterpart, where are nested

lmtest::lrtest(modp, mod.nbin)



# -->
# Negative Binomial model is significantly better




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
# Hurdle NB model is considered to be a better fit than the non-hurdle version



# ----------
# hurdle nbin  vs. hurdle nbin
vuong(mod.hnbin, mod.znbin)



# -->
# model1 (Hurdle NB model) is significantly better fit than zero-inflated NB model



# ------------------------------------------------------------------------------
# Compare AIC and BIC
# ------------------------------------------------------------------------------

LRstats(modp, mod.qpois, mod.nbin, mod.zpois, mod.znbin, mod.hpois, mod.hnbin, sortby = "BIC")


# -->
# For both of AIC and BIC, mod.hnbin model is the beset





