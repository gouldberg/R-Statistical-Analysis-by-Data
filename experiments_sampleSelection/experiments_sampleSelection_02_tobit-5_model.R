setwd("//media//kswada//MyFiles//R//experiments_sampleSelection")

packages <- c("dplyr", "sampleSelection")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Create 3 variate normal disturbances with correlation
# ------------------------------------------------------------------------------

# correlation between error(s) and error(o1) is 0.9 and between error(s) and error(o2) is 0.5
# The third correlation 0.1 takes care of the positive definiteness of the covariance matrix and does not affect the results
vc <- diag(3)

vc[lower.tri(vc)] <- c(0.9, 0.5, 0.1)

vc[upper.tri(vc)] <- vc[lower.tri(vc)]



# ----------
set.seed(0)

eps <- rmvnorm(500, c(0, 0, 0), vc)


eps


# ------------------------------------------------------------------------------
# variable for selection equation
# ------------------------------------------------------------------------------

# xs:  uniformly distributed explanatory variable for the selection equations
xs <- runif(500)


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
xo1 <- runif(500)
xo2 <- runif(500)


# yoX:  latent outcome variable
yoX1 <- xo1 + eps[, 2]
yoX2 <- xo2 + eps[, 3]



# ------------------------------------------------------------------------------
# tobit-5 model with exclusion restriction
# ------------------------------------------------------------------------------

library(sampleSelection)

tobit5_mod <- selection(ys ~ xs, list(yoX1 ~ xo1, yoX2 ~ xo2))

summary(tobit5_mod)

coef(tobit5_mod)


# -->
# We can see that the parameters are fairly well estimated.
# All the estimates are close to the true values.


