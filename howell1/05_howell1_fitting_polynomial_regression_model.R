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



# ------------------------------------------------------------------------------
# The relationship of weight and height:  visibly curved
# ------------------------------------------------------------------------------
plot(height ~ weight, data = d, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.6))



# ------------------------------------------------------------------------------
# fitting polynomial regression model
# ------------------------------------------------------------------------------
# standardize for easier interpretation and avoiding numerical glitches in very large values
d$weight.s <- (d$weight - mean(d$weight)) / sd(d$weight)

d$weight.s2 <- d$weight.s^2

mod_pl <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight.s + b2 * weight.s2,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d)



# These numbers provide Gaussian approximations fro each parameter's marginal distribution.
precis(mod_pl, corr = TRUE)



# ----------
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)

pred_dat <- list(weight.s = weight.seq, weight.s2 = weight.seq^2)
mu <- link(mod_pl, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(mod_pl, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight.s, data = d, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)



# ------------------------------------------------------------------------------
# fitting higher-order polynomial regression model
# ------------------------------------------------------------------------------
d$weight.s3 <- d$weight.s^3

mod_pl2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight.s + b2 * weight.s2 + b3 * weight.s3,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d)



# These numbers provide Gaussian approximations fro each parameter's marginal distribution.
precis(mod_pl2, corr = TRUE)



# ----------
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)

pred_dat <- list(weight.s = weight.seq, weight.s2 = weight.seq^2, weight.s3 = weight.seq^3)
mu <- link(mod_pl2, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(mod_pl2, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight.s, data = d, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)
