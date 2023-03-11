setwd("//media//kswada//MyFiles//R//milk")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  milk
# ------------------------------------------------------------------------------
data("milk", package = "rethinking")

d <- milk

dim(d)

str(d)



# ------------------------------------------------------------------------------
# remove NAs, and rescale one of the explanatory variables
# ------------------------------------------------------------------------------
d <- milk[complete.cases(milk), ]
d$neocortex <- d$neocortex.perc / 100
dim(d)



# ------------------------------------------------------------------------------
# 4 models
# ------------------------------------------------------------------------------
# We constrain the standard deviation of the outcome, sigma, to be positive
# The trick is to estimate the logarithm of sigma, which can be any real number. Then inside the likelihood function we just exponentiate it.

a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))


# The priors are assumed to be all flat here.
mod1 <- map(
  alist(
    kcal.per.g ~ dnorm(a, exp(log.sigma))
  ),
  data = d,
  start = list(a = a.start, log.sigma = sigma.start)
)



mod2 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn * neocortex
  ),
  data = d,
  start = list(a = a.start, bn = 0, log.sigma = sigma.start)
)



mod3 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bm * log(mass)
  ),
  data = d,
  start = list(a = a.start, bm = 0, log.sigma = sigma.start)
)



mod4 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn * neocortex + bm * log(mass)
  ),
  data = d,
  start = list(a = a.start, bm = 0, bn = 0, log.sigma = sigma.start)
)



# ------------------------------------------------------------------------------
# comparing WAIC values
# ------------------------------------------------------------------------------
WAIC(mod1)
WAIC(mod2)
WAIC(mod3)
WAIC(mod4)


# ----------
# WAIC: samller WAIC indicates better estimated out-of-sample deviance, so model 4 is ranked first
# pWAIC: the estimated effective number of parameters. This provides a clue as to how flexible each model is in fitting the sample
# dWAIC:  the difference between each WAIC and the lowest WAIC. Since only relative deviance matters, this column shows the differences in relative fashion.
# weight:  AKAIKE WEIGHT for each model. These values are transformed information criterion values
#  --> "A model's weight is an estimate of the probability that the model will make the best predictions on new data, conditional on the set of models considered"
#  Akaike weights convert WAIC(expected deviance of a model on future data), which are log-likelihoods, to plain likelihoods and then standardize them all.
# This is just like Bayes' theorem uses a sum in the denominator to standardize the product of the likelihood and prior.
# Therefore the Akaike weights are analogous to posterior probabilities of models, conditional on expected future data.
# SE: standard error of the WAIC estimate.
# dSE: the standard error of the difference in WAIC between each model and the top-ranked model.
milk.models <- compare(mod1, mod2, mod3, mod4)
milk.models


# -----------
# full set of the standard error of pairwise model differences in WAIC between each model
milk.models@dSE



# ----------
# filled points are the in-sample deviance of each model, which for WAIC is calculated as -2 * lppd, which is 2 * 2pWAIC from the corresponding WAIC value.
# The open points are WAIC
# dark line segment: the standard error of each WAIC
# gray triangles and line segments: the standard error of the difference between each WAIC and the top-ranked WAIC
# --> You need these triangles and gray lines, because the overlap in standard deviations (the dark lines) between models is not also a measure of the standard error in the difference between models

plot(milk.models, SE=TRUE, dSE=TRUE)



# -->
# The best model has more than 90% of the model weight. 
# But with only 12 cases, the error on the WAIC estimates is substantial, and that uncertainty should propagate to the Akaike weights.
# It's easy to imagine that additional data could reduce the strength of these associations. But the evidence is certainly consistent with the view that
# neocortex is positively associated with milk energy, controlling for body mass.
# Notice as well that either predictor alone is actually expected to do worse thatn the model without either predictor.



# ------------------------------------------------------------------------------
# comparing estimates
#   - Comparing estimates among models is useful to understand why a particular model or models have lower WAIC values. Changes in posterior distributions,
#"    across models, provide useful hints.
#   - Regardless of WAIC values, we often want to know whether some parameter's posterior distribution is stable across models.
# ------------------------------------------------------------------------------

coeftab(mod1, mod2, mod3, mod4, se=TRUE)


# each point is a MAP estimate and each black line segment is an 89% percentile interval.
plot(coeftab(mod1, mod2, mod3, mod4))



# ----------
# You can adjust these plots to group by model
coeftab_plot(coeftab(mod1, mod2, mod3, mod4), by.model=TRUE, prob = 0.89)



# ------------------------------------------------------------------------------
# Model averaging
# ------------------------------------------------------------------------------
# compute counterfactual predictions by best model (mod4)
nc.seq <- seq(from = 0.5, to = 0.8, length.out = 30)

d.predict <- list(
  kcal.per.g = rep(0, 30),
  neocortex = nc.seq,
  mass = rep(4.5, 30)
)


pred.mod4 <- link(mod4, data = d.predict)
mu <- apply(pred.mod4, 2, mean)
mu.PI <- apply(pred.mod4, 2, PI)


plot(kcal.per.g ~ neocortex, d, col = rangi2)
lines(nc.seq, mu, lty=2)
lines(nc.seq, mu.PI[1,], lty=2)
lines(nc.seq, mu.PI[2,], lty=2)


# ----------
# Compute an ensemble of posterior predictions
milk.ensemble <- ensemble(mod1, mod2, mod3, mod4, data = d.predict)
mu <- apply(milk.ensemble$link, 2, mean)
mu.PI <- apply(milk.ensemble$link, 2, PI)
lines(nc.seq, mu)
shade(mu.PI, nc.seq)


# -->
# THe regression line, which shows the average mu at each value horizontal axis, has hardly moved at all.
# The shaded interval includes a slope of zero.
# This is because the lower-ranked WAIC models, totaling only 10% of the Akaike weight -- all suggest a near-zero (or exactly zero) slope for neocortex.
# Retaining the model uncertainty, as estimated by Akaike weight here, helps guard against overconfidence.


