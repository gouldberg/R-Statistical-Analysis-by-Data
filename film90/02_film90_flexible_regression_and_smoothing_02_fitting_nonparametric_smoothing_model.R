setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss", "gamlss.add")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# P-splines:  nonparametric penalized smoothing splines
# ------------------------------------------------------------------------------

# pb() estimate automatically smoothing parameter (and therefore the effective degrees of freedom) using the default local maximum likelihood method
m1 <- gamlss(lborev1 ~ pb(lboopen), data = film90, family = NO)


summary(m1)



# ----------
# effective degrees of freedom (the effective number of parameters)
edf(m1)

edf(m1, "mu")



# ----------
plot(lborev1 ~ lboopen, data = film90, col = "lightblue")

lines(fitted(m1)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)])



# -->
# DO NOTE TRY TO INTERPRET THE LINEAR COEFFICIENTS OR THE STANDARD ERRORS OF THE SMOOTHING TERMS
# When fitting a smooth nonparametric term in gamlss() is that the displayed coefficient of the smoothing term and its standard error
# refer only to the linear component of the term.
# The linear part of the smoothing is fitted together with all other linear terms (in this case only intercept)


# One should try to interpret the whole smoothing function, which can be obtained using term.plot()



# ----------
# mu, scale parameter (sigma)
fitted(m1, "mu")
fitted(m1, "sigma")

# same
predict(m1, what = "sigma", type = "response")



# ------------------------------------------------------------------------------
# Cubic splines
# ------------------------------------------------------------------------------

m2 <- gamlss(lborev1 ~ cs(lboopen, df = 10), data = film90, family = NO)


summary(m2)


# total degrees of freedom = 13 = 1(constant) + 1(linear) + 10(smoothing) + 1(sigma)



# ----------
lines(fitted(m2)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "red", lty = 2, lwd = 2)

legend("topleft", legend = c("m1: P-plines", "m2: cubic splines"), lty = 1:2, col = c("black", "red"), cex = 1)



# ------------------------------------------------------------------------------
# Loess:  Locally weighted scatterplot smoothing
# ------------------------------------------------------------------------------

m4 <- gamlss(lborev1 ~ lo(~lboopen, span = .4), data = film90, family = NO)

summary(m4)



# ------------------------------------------------------------------------------
# Neural Networks
#   - Neural networks can be considered as another type of smoother.
#   - additive function nn() is part of the package gamlss.add
# ------------------------------------------------------------------------------

mnt <- gamlss(lborev1 ~ nn(~lboopen, size = 20, decay = 0.1), data = film90, family = NO)

summary(mnt)



# ----------
plot(lborev1 ~ lboopen, data = film90, col = "lightblue")
lines(fitted(m1)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)])
lines(fitted(m2)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "red", lty = 2)
lines(fitted(m4)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "blue", lty = 3)
lines(fitted(mnt)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "green", lty = 4)
legend("topleft", legend = c("m1: P-plines", "m2: cubic splines", "m4: loess", "mnt: neural network"), lty = 1:4, col = c("black", "red", "blue", "green"), cex = 1)



# ----------
# get more information about the fitted neural network model
getSmo(mnt)

summary(getSmo(mnt))


# Here we retrieve the 61 coefficients.
# 40 parameters from the relationship betwenn the 20 hidden variables and explanatory variable (constant and slope parameters)
# together with 21 parameters from the relationship between the response variable and the 20 hidden variables (constant and 20 slope parameters)
coef(getSmo(mnt))



# ------------------------------------------------------------------------------
# Modeling both mu and sigma
# ------------------------------------------------------------------------------

m3 <- gamlss(lborev1 ~ pb(lboopen), sigma.formula = ~ pb(lboopen), data = film90, family = NO)



# ----------
# obtain the effective degrees of freedom for all the distribution parameters
edfAll(m3)



# ----------
plot(lborev1 ~ lboopen, col = "lightblue", data = film90)
lines(fitted(m1)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "black", lty = 2)
lines(fitted(m3)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "blue", lty = 1)


