setwd("//media//kswada//MyFiles//R//experiments_sampleSelection")

packages <- c("dplyr", "sampleSelection")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Create 3 variate Chi-square disturbances with correlation
# ------------------------------------------------------------------------------

# correlation between error(s) and error(o1) is 0.9 and between error(s) and error(o2) is 0.5
# The third correlation 0.1 takes care of the positive definiteness of the covariance matrix and does not affect the results
vc <- diag(3)

vc[lower.tri(vc)] <- c(0.9, 0.5, 0.1)

vc[upper.tri(vc)] <- vc[lower.tri(vc)]


# ----------
# Create disturbances as 3-variate * X^2 (chi-square) random variables (subtracting 1 in order to get the mean zero disturbances)
eps <- rmvnorm(1000, rep(0, 3), vc)

eps <- eps^2 - 1

eps


# ------------------------------------------------------------------------------
# variable for selection equation
# ------------------------------------------------------------------------------

# generate xs to be in the interval [−1, 0] in order to get an asymmetric distribution over observed choices:
xs <- runif(1000, -1, 0)


# ys:  the selection outcome
ys <- xs + eps[, 1] > 0



# ----------
# selection equation
par(mfrow = c(1, 1))

plot(ys ~ xs, pch = 20)



# ------------------------------------------------------------------------------
# variable for outcome equation, latent outcome
# ------------------------------------------------------------------------------

# xo:  explanatory variable for the outcome equation
# Note that xo is independent from xs
# the exclusion restriction -- independent information about the selection process -- has a certain identifying power
xo1 <- runif(1000)
xo2 <- runif(1000)


# yoX:  latent outcome variable
yoX1 <- xo1 + eps[, 2]
yoX2 <- xo2 + eps[, 3]



# ------------------------------------------------------------------------------
# tobit-5 model with exclusion restriction
# ------------------------------------------------------------------------------

library(sampleSelection)

tobit5_mod2 <- selection(ys ~ xs, list(yoX1 ~ xo1, yoX2 ~ xo2), iterlim = 20)

summary(tobit5_mod2)


coef(tobit5_mod2)



# -->
# Although still have an exclusion restriction,
# now serious problems appear -- most intercepts are statistically significantly different from the true values zero.
# This model serious convergence problems.



# ------------------------------------------------------------------------------
# tobit-5 model without exclusion restriction
# ------------------------------------------------------------------------------


set.seed(6)

# slightly larger variance
xs <- runif(1000, -1, 1)

ys <- xs + eps[, 1] > 0

yo1 <- xs + eps[, 2]

yo2 <- xs + eps[, 3]


summary(tmp <- selection(ys ~ xs, list(yo1 ~ xs, yo2 ~ xs), iterlim = 10))


# -->
# the results are seriously biased.
# Note that the first outcome parameters have low standard errors, but a substantial bias.



# ----------
# for illustrative example:  same model with independent OLS equations
coef(summary(lm(yo1 ~ xs, subset = ys == 0)))

coef(summary(lm(yo2 ~ xs, subset = ys == 1)))


# -->
# One can see taht OLS estimates 

