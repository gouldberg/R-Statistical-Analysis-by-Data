setwd("//media//kswada//MyFiles//R//tulips")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tulips
# ------------------------------------------------------------------------------
data("tulips", package = "rethinking")

d <- tulips

dim(d)

str(d)



# ------------------------------------------------------------------------------
# regression modeling with interactions: un-centered models by default setting
# ------------------------------------------------------------------------------
# We'll user very flat priors here, so we get results nearly identical to typical maximum likelihood inference.
# And in this case, the range of the outcome variable, blooms, is huge. The minimum is zero, and the maximum is 362.
# So this means that priors that look very flat may not actually be, because "flat" is always relative to the likelihood.

# no interaction
mod1 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water + bS * shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d
)


# with interaction
mod2 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water + bS * shade + bWS * water * shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d
)



# ------------------------------------------------------------------------------
# regression modeling with interactions: un-centered models by specifying optim engine and the number of maximum iterations
# ------------------------------------------------------------------------------
# The default for map is called BFGS. This default is very capable, but it sometimes has trouble getting started.
# Two others to try, when you have trouble, are called Nelder-Mead and (when all else fails) SANN (simulated annealing).


# no interaction, optim engine is Nelder-Mead, maxit = 1e4
mod3 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water + bS * shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  method = "Nelder-Mead",
  control = list(maxit = 1e4)
)



# with interaction, optim engine is Nelder-Mead, maxit = 1e4
mod4 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water + bS * shade + bWS * water * shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  method = "Nelder-Mead",
  control = list(maxit = 1e4)
)



# ----------
coeftab(mod3, mod4)

precis(mod3, corr = TRUE, digits = 3)
precis(mod4, corr = TRUE, digits = 3)

compare(mod3, mod4)



# -->
# intercept:
#  - The estimate of the intercept changes a lot from one model to the next.
#  - In this case, neither of the predictor variables ever takes the value zero within the data.
#  - As a result, these intercept estimates are very hard to interpret.

# slopes:
#  - In the main-effect-only model, mod3, the MAP value for the main effect of water is positive and the main effect for shade is negative.
#    Both posterior distributions are reliably on one side of zero. You might infer that these posterior distributions suggest that 
#    water increases blooms while shade reduces them.

# But the analogous posterior distributions from the interaction model mod6 are quite different.
# Interaction model is indeed a much better model.
# Now both main effects are positive, but the new interaction posterior mean is negative.
# The negative interaction itself implies that as shade increases, water has a reduced impact on blooms.





