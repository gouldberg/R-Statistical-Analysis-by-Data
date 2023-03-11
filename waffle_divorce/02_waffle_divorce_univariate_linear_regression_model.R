setwd("//media//kswada//MyFiles//R//waffle_divorce")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Waffle Divorce
# ------------------------------------------------------------------------------
data("WaffleDivorce", package = "rethinking")

d <- WaffleDivorce

dim(d)

str(d)



# ------------------------------------------------------------------------------
# fitting single variable model: MedianAgeMarriage.s
# ------------------------------------------------------------------------------
# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)

mod1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)



# These numbers provide Gaussian approximations fro each parameter's marginal distribution.
# each additional standard deviation of delay in marriage (1.24 years) predicts a decrease of about one divorce per thousand adults, with a 89%
# interval from about -1.4 to -0.7.

sd(d$MedianAgeMarriage)
precis(mod1, corr = TRUE)



# ----------
# compute percentile interval of mean
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(mod1, data = data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = d, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))


# ----------
# The MAP line (just the posterior mean)
abline(a = coef(mod1)["a"], b = coef(mod1)["bA"])



# ----------
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)
shade(mu.HPDI, MAM.seq)



# ----------
# n <- 100
n <- 1e4
sim.divorce <- sim(mod1, data = list(MedianAgeMarriage.s = MAM.seq), n = n)
divorce.PI <- apply(sim.divorce, 2, PI, prob = 0.89)
shade(divorce.PI, MAM.seq)



# ------------------------------------------------------------------------------
# fitting single variable model: Marriage.s
# ------------------------------------------------------------------------------
# standardize predictor
d$Marriage.s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)

mod2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)



sd(d$Marriage)
precis(mod2, corr = TRUE)



# ----------
# compute percentile interval of mean
M.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(mod2, data = data.frame(Marriage.s = M.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ Marriage.s, data = d, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))


# ----------
# The MAP line (just the posterior mean)
abline(a = coef(mod2)["a"], b = coef(mod2)["bR"])


# ----------
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)
shade(mu.HPDI, M.seq)



# ----------
# n <- 100
n <- 1e4
sim.divorce <- sim(mod2, data = list(Marriage.s = M.seq), n = n)
divorce.PI <- apply(sim.divorce, 2, PI, prob = 0.89)
shade(divorce.PI, M.seq)


# -->
# This shows an increase of 0.6 divorces for every additional standard deviation of marraige rate (3.8).

