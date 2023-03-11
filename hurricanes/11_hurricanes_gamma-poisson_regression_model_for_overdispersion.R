# https://github.com/jffist/statistical-rethinking-solutions/blob/master/ch11_hw.R

setwd("//media//kswada//MyFiles//R//hurricanes")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------
data("Hurricanes", package = "rethinking")

d <- Hurricanes

dim(d)

str(d)


# ----------
car::some(d)

summary(d$deaths)



# ------------------------------------------------------------------------------
# poisson regression model considering overdispersion (gamma-Poisson (aka negative-binomial))
# ------------------------------------------------------------------------------
# ganna dustrubution:  more dispersed around its mean.
# As theta approaches zero, the gamma distribution approaches a Gaussian distribution with the same mean value.
mu <- 3
theta <- 2
curve(dgamma2(x, mu, theta), from = 0, to = 10)


# This model estimates the shape of a gamma distribution to describe the Poisson rates across cases
# The gamma-Poisson model is very much like a beta-binomial model, with the gamma distribution of rates (or expected values)
# replacing the beta distribution of probabilities of success.

mod3 <- map(
  alist(
    deaths ~ dgampois(mu, theta),
    log(mu) ~ a + b_fem * femininity,
    b_fem ~ dnorm(0, 1),
    a ~ dnorm(0, 1),
    theta ~ dexp(1)
  ),
  data=d
)


precis(mod3, digits = 3)

exp(0.027)



# ----------
# now b_fem is centered around zero, so looks like no correlation with deaths
coef <- extract.samples(mod3)
dens(coef$b_fem)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

# NOTE THAT:  you should NOT use WAIC with over-dispersion model, unless you are very sure of what you are doing.
# The reason is that a veta-binomial or gamma-Poisson likelihood applies an unobserved parameter to each row in the data,
# When we then go to calculate log-likelihoods, how the data are structured will determine how the veta-distributed or gamma-distributed variation
# enters the model.

( cmp <- compare(mod1, mod2, mod3) )


par(mfrow = c(1,2))
plot(cmp)
plot(coeftab(mod1, mod2, mod3))



# ------------------------------------------------------------------------------
# posterior validation check:  posterior distribution of deaths by each hurricane
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
postcheck(mod3, window = 100)


# Overdispersed model, as expected, has a wide interval for predictions of counts. 
# These intervals cover actual counts for almost all cases, except 8 cases with the number of deaths greater than 50. 
# Also all predictions are nearly the same and are close to the average of the sample.


# ------------------------------------------------------------------------------
# counter factual plot
#   - Because model contains only a single variable, we can draw counter factual plot to illustrate dependency
# ------------------------------------------------------------------------------
d.predict <- data.frame(femininity=seq(1,11,0.1))

lambda.sample <- link(mod3, d.predict)
lambda.avg <- apply(lambda.sample, 2, mean )
lambda.pi <- apply(lambda.sample, 2, PI )


# predict actual counts
count.sample <- sim(mod3, data = d.predict)
count.avg <- apply(count.sample, 2, mean )
count.pi <- apply(count.sample, 2, PI )


# plot
plot(d$femininity, d$deaths, xlim=c(0,12), col='blue', pch=16)
lines(d.predict$femininity, lambda.avg)
shade(lambda.pi, d.predict$femininity) #gives very narrow shade

lines(d.predict$femininity, count.avg, col='red')
shade(count.pi, d.predict$femininity)


