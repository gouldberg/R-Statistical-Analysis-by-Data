setwd("//media//kswada//MyFiles//R//chimpanzees")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prosocial chimpanzees
# ------------------------------------------------------------------------------
data("chimpanzees", package = "rethinking")

d <- chimpanzees

dim(d)

str(d)



# ------------------------------------------------------------------------------
# binomial regression:  modeling individual variation by MCMC
# ------------------------------------------------------------------------------
# clean NAs from the data
d2 <- d
d2$recipient <- NULL


unique(d$actor)


# Think of handedness here as a maskng variable. Estimate handedness as a distinct intercept for each individual
mod4_MCMC <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC * condition) * prosoc_left,
    a[actor] ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d2, chains = 2, iter = 2500, warmup = 500
)



# Note that these values are on the scale of log-oods
precis(mod4_MCMC, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# binomial regression:  modeling individual variation by quadratic approximation
# ------------------------------------------------------------------------------
mod4_qa <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC * condition) * prosoc_left,
    a[actor] ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d2
)


precis(mod4_qa, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
# In quadratic approximation, coef a[2] overlaps zero 
par(mfrow=c(1,1))
plot(coeftab(mod4_MCMC, mod4_qa))



# ----------
# In quadratic approximation, distribution of a[2] is Gaussian, but in MCMC it is really left-skewed.
pairs(mod4_MCMC)

pairs(mod4_qa)



# ----------
logistic(11)



# The main difference lies in the posterior distribution of the intercept for the second monkey(a[2]).
# It is highly skewed according to the Stan estimations but is symmetric in the quadratic estimations. 
# All other coefficients have similar estimated distribution.
# Intercept a[2] has large valuer and long tail because the second actor always pulled left lever, 
# so inference forces intercept to be as large as possible. E.g. logistic(MAP(a[2]))=logistic(11)=0.99998

