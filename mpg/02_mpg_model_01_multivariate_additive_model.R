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
# Initial multivariate additive model
# ------------------------------------------------------------------------------

# From physical principles (see, e.g., the excellent MacKay, 2008)
# we would expect that highway fuel consumption would be dominated by erodynamic factors, and hence would be best modelled in terms of car dimensions
# (frontal area in particular), 
# while city fuel consumption should be dominated by weight and other engine characteristics.

mod <- gam(list(city.mpg ~ fuel + style + drive + s(weight) + s(hp) + s(make, bs = "re"),
                hw.mpg ~ fuel + style + drive + s(weight) + s(hp) + s(make, bs = "re")),
           family = mvn(d = 2), data = mpg)


summary(mod)



plot(mod)



# -->
# The random effect of make is estimated as effectively zero for highway driving
# whereas make seems to matter for city driving.

# The smooths for weight and hp have strikingly similar shapes for city and highway,
# and the obvious question arises of whether we could simplify the model, by using the same smooths for both components.

