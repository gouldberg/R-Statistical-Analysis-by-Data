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
# Fit an additive smoothing model for the mu parameter
# ------------------------------------------------------------------------------

m11 <- gamlss(lborev1 ~ pb(lboopen) + pb(lnosc) + dist, data = film90, family = NO)



# ----------
# obtain the effective degrees of freedom for all the distribution parameters
edfAll(m11)



# ----------
# check the residulas
plot(m11, xvar = film90$lboopen)


# -->
# The model is particularly poor in terms of capturing the skewness and kurtosis



# ----------
# Reconfirm the misfits using worm plots

wp(m11, xvar = ~lboopen + lnosc, n.inter = 2, ylim.worm = 4)


# -->
# From bottom left to top right in rows, the first and last worm plots show significant misfits of the kurtosis
# while the third plot shows a misfit with respect to skewness.



# ------------------------------------------------------------------------------
# Fit the Box-Cox power exponential (BCPE) distribution: 4 parameter continuous distribution
# ------------------------------------------------------------------------------

# relatively simple model
m12 <- gamlss(lborev1 ~ pb(lboopen) + pb(lnosc) + dist,
              sigma.formula = ~ pb(lboopen), nu.formula = ~ pb(lboopen), tau.formula = ~ pb(lboopen), 
             data = film90, family = BCPE)


edfAll(m12)



# ----------
# check the residulas
plot(m12, xvar = film90$lboopen)

wp(m12, xvar = ~lboopen + lnosc, n.inter = 2, ylim.worm = 2)



# -->
# Although the residuals show a significantly improved model, it seems that there is a problem with the skewness, particularly for the second
# and third worm plots.



# ----------
# Thus we fit a new model for the nu parameter

m13 <- gamlss(lborev1 ~ pb(lboopen) + pb(lnosc) + dist,
              sigma.formula = ~ pb(lboopen), nu.formula = ~ pb(lboopen) + pb(lnosc), tau.formula = ~ pb(lboopen), 
              data = film90, family = BCPE, start.from = m12)


edfAll(m13)



# ----------
# check the residulas
plot(m13, xvar = film90$lboopen)

wp(m13, xvar = ~lboopen + lnosc, n.inter = 2, ylim.worm = 1)



# -->
# Second and third worm plots are problematic in terms of the mean (second) and kurtosis (third)



# ----------
# we fit a new model

m14 <- gamlss(lborev1 ~ pvc(lboopen, by = dist) + pvc(lnosc, by = dist) + dist,
              sigma.formula = ~ pb(lboopen), nu.formula = ~ pb(lboopen) + pb(lnosc), tau.formula = ~ pb(lboopen) + pb(lnosc), 
              data = film90, family = BCPE, start.from = m13)


edfAll(m14)



# ----------
# check the residulas
plot(m14, xvar = film90$lboopen)

wp(m14, xvar = ~lboopen + lnosc, n.inter = 2, ylim.worm = 1)



# -->
# Still have a problem with the mu parameter because of the second worm plot



# ----------
# Use a normality test for the residuals
# Shapiro-Wilk normality test
shapiro.test(resid(m14))


library(nortest)

# Anderson-Darling normality test, Cramer-von Mises normality test
ad.test(resid(m14))
cvm.test(resid(m14))



# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------

GAIC(m6, m11, m12, m13, m14)



# ----------
# compare the model
plot(lborev1 ~ lboopen, col = "lightblue", data = film90)
lines(fitted(m6)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "black", lty = 1)
lines(fitted(m12)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "black", lty = 2)
lines(fitted(m14)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "blue", lty = 3)



