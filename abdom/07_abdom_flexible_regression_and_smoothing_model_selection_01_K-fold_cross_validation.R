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
# K-fold cross validation
# ------------------------------------------------------------------------------

set.seed(123)


# ----------
# allocate the n = 610 observations to K = 10 cross-validation subsets
rand1 <- sample(10, 610, replace = TRUE)


g1 <- gamlssCV(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = NO, rand = rand1)


g2 <- gamlssCV(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = LO, rand = rand1)


g3 <- gamlssCV(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = TF, rand = rand1)



# ----------
CV(g1, g2, g3)


# -->
# The t distribution (g3) is selected.
# (This choice may reverse under another random allocation of observations to subsets)

