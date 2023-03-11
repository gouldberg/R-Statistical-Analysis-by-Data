setwd("//media//kswada//MyFiles//R//howell1")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Howell1
# ------------------------------------------------------------------------------
data("Howell1", package = "rethinking")

d <- Howell1

dim(d)

str(d)


d2 <- d[d$age >= 18, ]



# ------------------------------------------------------------------------------
# fitting maximum a posteriori linear model
# ------------------------------------------------------------------------------
mod_l <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d2)



# These numbers provide Gaussian approximations fro each parameter's marginal distribution.
precis(mod_l)



# ------------------------------------------------------------------------------
# inspect correlations among parameters
# ------------------------------------------------------------------------------
precis(mod_l, corr = TRUE)


# -->
# It just means that alpha and beta carry the same information -- as you change the slope of the line, the best intercept changes to match it.
# But in more complex models, strong correlations like this can make it difficult to fit the model to the data.



# ------------------------------------------------------------------------------
# data transformation by centering
# ------------------------------------------------------------------------------
d2$weight.c <- d2$weight - mean(d2$weight)

mod_lc <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight.c,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d2)



# These numbers provide Gaussian approximations fro each parameter's marginal distribution.
# The estimates for beta and sigma are unchanged (within rounding error), but the estimate for alpha is now the same as the average height value
# in the raw data.
precis(mod_lc, corr = TRUE)



# ------------------------------------------------------------------------------
# Plotting posterior inference against the data
#  - The MAP line
# ------------------------------------------------------------------------------
plot(height ~ weight, data = d2, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.6))


# ----------
# The MAP line (just the posterior mean)
abline(a = coef(mod_l)["a"], b = coef(mod_l)["b"])



# ------------------------------------------------------------------------------
# Plotting posterior inference against the data
#   - the cloud of regression lines
# ------------------------------------------------------------------------------
# Each row is a correlated random sample from the joint posterior of all three parameters, using the covariances provided by vcov(mod_l)
vcov(mod_l)

post <- extract.samples(mod_l)

post[1:5,]



# ----------
# 4 patterns of sample N
par(mfrow=c(2,2))
for(N in c(10,50,150,352)){
  dN <- d2[1:N,]
  
  mN <- map(
    alist(
      height ~ dnorm(mu, sigma),
      mu <- a + b * weight,
      a ~ dnorm(178, 100),
      b ~ dnorm(0, 10),
      sigma ~ dunif(0, 50)
    ), data = dN)
  
  n <- 20
  post <- extract.samples(mN, n = n)
  
  plot(dN$weight, dN$height, xlim = range(d2$weight), ylim = range(d2$height), cex = 0.5, pch = 16, col = rangi2, xlab = "weight", ylab = "height")
  mtext(concat("N = ", N))
  
  for(i in 1:20) abline(a = post$a[i], b = post$b[i], col = col.alpha("black", 0.3))
}


# -->
# Notice that the cloud of regression lines grows more compact as the sample size increases.
# This is a result of the model growing more confident about the location of the mean.


# ------------------------------------------------------------------------------
# Plotting posterior inference against the data
#   - plotting regression intervals and contours
# ------------------------------------------------------------------------------
post <- extract.samples(mod_l)

mu_at_50 <- post$a + post$b * 50

par(mfrow=c(1,1))
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu | weight = 50")

HPDI(mu_at_50, prob = 0.89)



# ----------
# rethinking::link function take your map model fit, sample from the posterior distribution, and then compute mu for each case
# in the data and sample from the posterior distribution.
# Each row is a sample from the posterior distribution
# The default is 1000 samples.
# There are 352 rows in d2, corresponding to 352 individuals, so there are 352 columns in the matrix mu.
mu <- link(mod_l)

str(mu)



# ----------
weight.seq <- seq(from = 25, to = 70, by = 1)

mu <- link(mod_l, data = data.frame(weight = weight.seq))
str(mu)


# type = "n" to hide raw data
plot(height ~ weight, data = d2, type = "n")

for(i in 1:100) points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))



# ----------
# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)

mu.mean
mu.HPDI



# ----------
# now with 89% HPDI of the mean indicated by the shaded region
plot(height ~ weight, data = d2, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)



# ------------------------------------------------------------------------------
# prediction intervals
# ------------------------------------------------------------------------------
# simulated heights, not distributions of plausible average height mu:
# For any unique weight value, you sample from a Gaussian distribution with the correct mean mu for that weight,
# using the correct value of sigma sampled from the same posterior distribution.
# If you do this for every sample from the posterior, for every weight value of interest, you end up with a collection of simulated heights
# that embody the uncertainty in the posterior as well as the uncertainty in the Gaussian likelihood.

# n <- 100
n <- 1e4
sim.height <- sim(mod_l, data = list(weight = weight.seq), n = n)

str(sim.height)

height.PI <- apply(sim.height, 2, PI, prob = 0.89)



# ----------
# The wide shaded region in the figure represents the area within which the model expects to find 89% of actual heights in the population, at each weight.
plot(height ~ weight, data = d2, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)



