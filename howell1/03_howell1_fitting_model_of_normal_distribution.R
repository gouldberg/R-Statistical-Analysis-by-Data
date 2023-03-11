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
# fitting maximum a posteriori model
#  - map estimates the posterior by climbing it like a hill. 
#    Unless you tell it otherwise, map starts at random values sampled from the prior.
# ------------------------------------------------------------------------------
# list evalueates the code, while alist does not.
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)



# ----------
# fit maximum a posteriori model
mod1 <- map(flist, data = d2)


# These numbers provide Gaussian approximations fro each parameter's marginal distribution.
precis(mod1)



# ------------------------------------------------------------------------------
# sampling from a map fit
# ------------------------------------------------------------------------------
post <- extract.samples(mod1, n = 1e4)

head(post)



# ----------
precis(post)
precis(post_small)



# ----------
plot(post$mu, post$sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))



# ----------
# show a normal approximation with the same mean and variance
#  --> longer tail at the top of the cloud of points, long tail of uncertainty towards higher values
dens(post$sigma, norm.comp = TRUE)



# ------------------------------------------------------------------------------
# fitting maximum a posteriori model
#   - The quadratic assumption for sigma can be problematic since variance is non-negative.
#     A conventional way to improve the situation is the estimate log(sigma) instead.
#     If we impose the quadratic approximation on the logarithm, rather than the standard deviation itself, we can often get
#     a better approximation of the uncertainty.
#   - When you have a lot of data, this won't make any noticeable difference, but the use of exp to effectively constrain a parameter to be positive
#     is a robust and usefule one.
# ------------------------------------------------------------------------------

mod1_logsigma <- map(
  alist(
    height ~ dnorm(mu, exp(log_sigma)),
    mu ~ dnorm(178, 20),
    log_sigma ~ dnorm(2, 10)
  ), data = d2)

post_logsigma <- extract.samples(mod1_logsigma)
sigma <- exp(post_logsigma$log_sigma)

dens(sigma, norm.comp = TRUE)



