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
# Local estimation of the smoothing parameters
#   - Local in which the methods are applied within the GAMLSS algorithm
# ------------------------------------------------------------------------------

# fitting the model with pb()

a1 <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = LO)


a2 <- gamlss(y ~ pb(x, method = "GCV"), sigma.fo = ~pb(x, method = "GCV"), data = abdom, family = LO)


a3 <- gamlss(y ~ pb(x, method = "GAIC", k = 2), sigma.fo = ~pb(x, method = "GAIC", k = 2), data = abdom, family = LO)



# ----------
# the effective degrees of freedom used
edfAll(a1)

edfAll(a2)

edfAll(a3)



# ------------------------------------------------------------------------------
# Glabal estimation of the smoothing parameters
#   - Global in which the methods are applied outside the GAMLSS algorithm
#   - Glabol GAIC method to find the degrees of freedom.
#     find.hyper() minimizes GAIC with k = 2 by default.
# ------------------------------------------------------------------------------

mod1 <- quote(gamlss(y ~ pb(x, df = p[1]), sigma.fo = ~ pb(x, df = p[2]), family = LO, data = abdom))


# The initial values for the parameters p are both set to 3.
# The minimum value for p for the search is set to 0 (lower = c(0, 0) 
# the steps in each parameter p used within the optim() search to 0.1
# The default method used by optim() within find.hyper() is "L-BFGS-B"
op <- find.hyper(model = mod1, par = c(3,3), lower = c(0,0), steps = c(0.1, 0.1))


op


# -->
# The resulting extra degrees of freedom are 3.71 for mu and zero for sigma,
# corresponding to total degrees of freedom 5.71 for mu and 2 for sigma.

# These are close to the results obtained using the default local "ML" method in pb() in model a1.

