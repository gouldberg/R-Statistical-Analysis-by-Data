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
# Correlation
# ------------------------------------------------------------------------------
# The variables perc.fat and perc.lactose contain much of the same information. They are substitutes for one another.
pairs(~ kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)


cor(d$perc.fat, d$perc.lactose)



# ------------------------------------------------------------------------------
# Bivariate models and multivariate model
# ------------------------------------------------------------------------------
# kcal.per.g regressed on perc.fat
mod4 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)



# kcal.per.g regressed on perc.lactose
mod5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl * perc.lactose,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)



precis(mod4, corr = TRUE, digits = 3)
precis(mod5, corr = TRUE, digits = 3)


# -->
# The posterior means of perc.fat and perc.lactose are essentially mirror images of one another, with the posterior mean of
# of being as positive as the mean of bl is negative.
# Both are narrow posterior distributions that lie almost entirely on one side or the other of zero.



# ----------
# both in the model:  WHEN ADDING VARIABLES HURT
mod6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat + bl * perc.lactose,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)


precis(mod6, corr = TRUE, digits = 3)


# -->
# Now the posterior means of both bf and bl are closer to zero.
# And the standard deviations for both parameters are twice as large as in the bivariate models (mod4 and mod5).
# In the case of percent fat, the posterior mean is essentially zero.
# The variables perc.fat and perc.lactose contain much of the same information. They are substitutes for one another.
# As a result, when you include both in a regression, the posterior distribution ends up describing a long ridge of combinations of bf and bl that are equally plausible.


# It is not always true that highly correlated variables are completely redundant -- other predictors might be correlated with only one of the pair,
# and so help extract the unique information each predictor provides.
# So you can not know just from a table of correlations nor from a matrix of scatterplots whether multicollinearity will prevent you from
# including sets of variables in the same model.


# ------------------------------------------------------------------------------
# Simulating collinearity
# ------------------------------------------------------------------------------

sim.coll <- function(r = 0.9){
  d$x <- rnorm(nrow(d), mean = r * d$perc.fat, sd = sqrt(1 - r^2) * var(d$perc.fat))
  m <- lm(kcal.per.g ~ perc.fat + x, data = d)
  sqrt(diag(vcov(m)))[2]
}


rep.sim.coll <- function(r = 0.9, n = 1000){
  stddev <- replicate(n, sim.coll(r))
  mean(stddev)
}


r.seq <- seq(from = 0, to = 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r = z, n = 1000))


# ----------
plot(stddev ~ r.seq, type = "l", col = rangi2, lwd = 2, xlab = "correlation")


# -->
# This code uses implicit flat priors, which are bad priors.
# So it does exaggerate the effect of collinear variables.
# When you use informative priors, the inflation in standard deviation can be much slower.



# ------------------------------------------------------------------------------
# Ref.: model comparison
# ------------------------------------------------------------------------------

compare(mod1, mod2, mod3, mod4, mod5, mod6)
