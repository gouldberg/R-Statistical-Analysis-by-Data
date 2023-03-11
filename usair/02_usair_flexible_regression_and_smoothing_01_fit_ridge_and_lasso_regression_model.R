setwd("//media//kswada//MyFiles//R//usair")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usair
# ------------------------------------------------------------------------------
data("usair", package = "gamlss.data")


str(usair)

car::some(usair)



# ------------------------------------------------------------------------------
# Fit Ridge and Lasso regression terms by ri()
# ------------------------------------------------------------------------------

# The design matrix of the explanatory variables (whose columns are standardized automatically to mean zero and standard deviation one)
X <- with(usair, cbind(x1, x2, x3, x4, x5, x6))



# Least squares model with scaling
m0 <- gamlss(y ~ scale(X), data = usair)



# Ridge regression
m1 <- gamlss(y ~ ri(X), data = usair)



# Lasso regression
m2 <- gamlss(y ~ ri(X, Lp = 1), data = usair)



# Best subset
m3 <- gamlss(y ~ ri(X, Lp = 0), data = usair)



# ----------
AIC(m0, m1, m2, m3)



# -->
# Lasso seems most appropriate here.



# ----------
# The different coefficients of the four fitted models
cbind(
  zapsmall(coef(m0)[-1], digits = 4),
  zapsmall(coef(getSmo(m1)), digits = 3),
  zapsmall(coef(getSmo(m2)), digits = 3),
  zapsmall(coef(getSmo(m3)), digits = 3)
)


# -->
# Ridge regression (Lp = 2) shrinks most of the least squares coefficients towards zero.
# Lasso regression (Lp = 1) does the same, but also sets the coefficient of x6 to zero.
# The best subset, Lp = 0, sets four coefficients to zero leaving only x1 and x2 whose coefficients are substantially shrunk towards zero.


# ----------
# fitted coefficients from the 3 different shrinkage medhos
plot(getSmo(m1))
plot(getSmo(m2))
plot(getSmo(m3))



