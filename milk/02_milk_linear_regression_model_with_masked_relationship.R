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
# fitting simple bivariate regression between kilocalories and neocortex percent
# ------------------------------------------------------------------------------
dcc <- d[complete.cases(d),]


mod1 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc)



# Note that 89% confidence interval of a and bn overlaps zero
precis(mod1, corr = TRUE, digits = 3)



# ----------
# compute percentile interval of mean
np.seq <- 0:100
mu <- link(mod1, data = data.frame(neocortex.perc = np.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


par(mfrow=c(1,1))
plot(kcal.per.g ~ neocortex.perc, data = dcc, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)


# -->
# The MAP line is weakly positive, but it is imprecise.
# A log of mildly positive and negative slopes are plausible, given this model and these data.



# ------------------------------------------------------------------------------
# the logarithm of body mass as the predictor
# ------------------------------------------------------------------------------
dcc$log.mass <- log(dcc$mass)


mod2 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm * log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc)


# Still confidence interval of bm overlaps zero
precis(mod2, corr=TRUE, digits = 3)


# -->
# Log-mass is negatively correlated with kilocalories. This influence does seem stronger than that of neocortex percent,
# although in the opposite direction.


# ----------
# compute percentile interval of mean
np.seq <- -2:4
mu <- link(mod2, data = data.frame(log.mass = np.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


plot(kcal.per.g ~ log.mass, data = dcc, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)



# ------------------------------------------------------------------------------
# Both predictors
# ------------------------------------------------------------------------------
mod3 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc + bm * log.mass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc)


precis(mod3, corr=TRUE, digits = 3)


# -->
# By incorporating both predictor variables in the regression, the estimated assocation of both with the outcome has increased.
# The posterior mean for the association of neocortex percent has increased more than sixfold, and its 89% interval is now entirely above zero.
# The posterior mean for log body mass is more strongly negative.


# ----------
mean.log.mass <- mean(log(dcc$mass))

np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc = np.seq,
  log.mass = mean.log.mass
)


mu <- link(mod3, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


# counterfactual plots
plot(kcal.per.g ~ neocortex.perc, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)


# ----------
mean.neocortex.perc <- mean(dcc$neocortex.perc)

np.seq <- -2:4
pred.data <- data.frame(
  neocortex.perc = mean.neocortex.perc,
  log.mass = np.seq
)


mu <- link(mod3, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


plot(kcal.per.g ~ log.mass, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)



# -->
# Bigger species, like apes, have milk with less energy.
# But species with more neocortex tend to have richer milk.



# ----------
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)
shade(mu.HPDI, MAM.seq)



