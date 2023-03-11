setwd("//media//kswada//MyFiles//R//mpg")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 7. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mpg
# ------------------------------------------------------------------------------

data(mpg)

str(mpg)

dim(mpg)



# ------------------------------------------------------------------------------
# Multivariate additive model with joint smoothers
# ------------------------------------------------------------------------------

# Use same smooths of weight and hp for both mpg measurements, but has an extra smooth of both for the city.mpg.
# If the joint smooths are adequate then the extra smooths should turn out to be unnecessary.
# The third formula specifies the shared term.

mod1 <- gam(list(city.mpg ~ fuel + style + drive + s(hp) + s(weight) + s(make, bs = "re"),
                hw.mpg ~ fuel + style + drive + s(make, bs = "re"),
                1 + 2 ~ s(weight) + s(hp) - 1),
           family = mvn(d = 2), data = mpg)


summary(mod1)



plot(mod1)



# -->
# There are some significant (non constant) differences between the city and highway responses to weight and hp.
# The way that city efficiency initially goes up with hp, relative to highway, is initially counter intuitive,
# but probably reflects some relative inefficiency of low powered cars in highway driving.

# The initial extra effect of weight is as expected  --  extra weight adds disproportionately more consumption in city driving.



# -----------
# In fact mod1 has a slightly higher AIC than mod, so there is not really any good reason to prefer it to the original model.
AIC(mod, mod1)

