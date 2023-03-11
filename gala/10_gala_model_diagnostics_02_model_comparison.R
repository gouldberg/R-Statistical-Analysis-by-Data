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
# Model Comparison by Likelihood Ratio Test
# ------------------------------------------------------------------------------

# We can caompre a given Poisson model to its negative-binomial counterpart, where are nested

lmtest::lrtest(modp2, mod.nbin)



# -->
# Poisson model is significantly better



# ------------------------------------------------------------------------------
# Compare AIC and BIC
# ------------------------------------------------------------------------------

LRstats(modp.step2, modp2, mod.qpois, mod.nbin, sortby = "BIC")


# -->
# mod.nbin is much better both in AIC and BIC





