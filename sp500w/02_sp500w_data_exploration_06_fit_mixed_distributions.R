setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)


# convert to ts object
sp500w_c <- ts(sp500w, start = 2003, freq = 52)




# ------------------------------------------------------------------------------
# data exploration:  fit mixed distributions
# ------------------------------------------------------------------------------


library(gamlss.mx)


# mixture = "np":  nonparametric finite mixtures
# sigma.fo = ~ MASS:  different sigma for each

sp.1 <- gamlssNP(formula = Close ~ 1, mixture = "np", K = 1, tol = 1, data = sp500w, family = NO, plot.opt = 0)

sp.2 <- gamlssNP(formula = Close ~ 1, sigma.fo = ~MASS, mixture = "np", K = 2, tol = 1, data = sp500w, family = NO, plot.opt = 0)

sp.3 <- gamlssNP(formula = Close ~ 1, sigma.fo = ~MASS, mixture = "np", K = 3, tol = 1, data = sp500w, family = NO, plot.opt = 0)

sp.4 <- gamlssNP(formula = Close ~ 1, sigma.fo = ~MASS, mixture = "np", K = 4, tol = 1, data = sp500w, family = NO, plot.opt = 0)




# ----------
# GAIC(sp.1, sp.2, sp.3, sp.4)
GAIC(sp.1, sp.2, sp.3)


# GAIC(sp.1, sp.2, sp.3, sp.4, k = log(length(sp500w)))
GAIC(sp.1, sp.2, sp.3, k = log(length(sp500w)))



# -->
# best model:  mixed 2 or 3 normal distributions




# ----------
sp.3


# -->
# Model sp.3 can be presented as Y ~ NO(mu, sigma) where
# mu1 = -0.002134              with probability 0.02988655
# mu2 = -0.002134 + 0.006751   with probability 0.5512507
# mu3 = -0.002134 - 0.001458   with probability 0.4188628

# and sigma1 = exp(-2.518) = 0.081
# and sigma2 = exp(-2.518 - 1.851) = 0.0127
# and sigma3 = exp(-2.518 - 0.984) = 0.0301



# ----------
# The estimated posterior (conditional) probabilities
head(sp.3$post.prob[[1]])




# ----------
# residual plot of the finite mixture model
plot(sp.3)




# ----------
# model diagnostics

wp(sp.3)




# ------------------------------------------------------------------------------
# by gamllMX
# ------------------------------------------------------------------------------

# IT TAKES TIME: 1 min.
fit_gmx <- gamlssMXfits(formula = Close ~ 1, K = 3, sigma.fo = ~ MASS, data = sp500w, family = NO, plot.opt = 0)


fit_gmx




# ------------------------------------------------------------------------------
# Plot fitted values
# ------------------------------------------------------------------------------

week_x <- seq(-0.3, 0.3, by = 0.001)


mu <- c(-0.002344, 0.004639, -0.003436)

sig <- c(exp(-2.51), exp(-4.379), exp(-3.507))

pi <- c(0.02929457, 0.5428803, 0.4278252)



fyNO <- dMX(y = week_x, 
            mu = mu,
            sigma = sig,
            pi = pi,
            family = list("NO", "NO", "NO"))


plot(fyNO ~ week_x, type="l")  




# ----------
# 3 mixed normal distribution image

fyNO1 <- dnorm(x = week_x,mean = mu[1], sd = sqrt(sig[1]))

fyNO2 <- dnorm(x = week_x,mean = mu[2], sd = sqrt(sig[2]))

fyNO3 <- dnorm(x = week_x,mean = mu[3], sd = sqrt(sig[3]))


plot(week_x, fyNO1, type="l", lty = 2, col = "black", 
     xlim = c(-0.3, 0.3), ylim = c(0, 5), xlab = "weekly return", ylab = "",
     main = "3 mixed normal distribuion", cex.main = 2)
lines(week_x, fyNO2, type="l", lty = 1, col = "blue")
lines(week_x, fyNO3, type="l", lty = 3, col = "red")




# ----------
fn <- getpdfMX(fit_gmx)


par(mfrow = c(1,1))

MASS::truehist(sp500w$Close, nbins = 30, col = "grey", xlab = "weekly return")

lines(week_x, fn(week_x), lty = 1, lwd = 2, col = "blue")


