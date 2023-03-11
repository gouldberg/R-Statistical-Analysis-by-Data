setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# simple linear regression model with normal errors
# ------------------------------------------------------------------------------

m <- gamlss(lborev1 ~ lboopen, data = film90, family = NO)


summary(m)



# ----------
plot(lborev1 ~ lboopen, data = film90, col = "lightblue")

lines(fitted(m) ~ film90$lboopen)



# ------------------------------------------------------------------------------
# cubic polynomial
# ------------------------------------------------------------------------------

m00 <- gamlss(lborev1 ~ lboopen + I(lboopen^2) + I(lboopen^3), data = film90, family = NO)


summary(m00)



# ----------
# Note that for large data sets it could be more efficient (and may be essential) to calculate the polynomial terms in advance
# prior to using the gamlss() function
# and then use them within the gamlss() function, since the evaluation is then done only once.

film90_t <- transform(film90, lb2 = lboopen^2, lb3 = lboopen^3)

head(film90)
head(film90_t)


m002 <- gamlss(lborev1 ~ lboopen + lb2 + lb3, data = film90_t, family = NO)

summary(m002)


# sigma is log linked --> exp is required
exp(coef(m002, "sigma"))



# ----------
plot(lborev1 ~ lboopen, data = film90, col = "lightblue")

lines(fitted(m002)[order(film90_t$lboopen)] ~ film90_t$lboopen[order(film90_t$lboopen)])



# ------------------------------------------------------------------------------
# standard errors
# ------------------------------------------------------------------------------

# the variance-covariance matrix of the parameters
print(vcov(m00), digit = 3)



# ----------
# the correlation matrix
print(vcov(m00, type = "cor"), digit = 3)



# ----------
# standard errors
print(vcov(m00, type = "se"), digit = 2)


# robust ("sandwich" or "Huber sandwich") standard errors
print(vcov(m00, type = "se", robust = TRUE), digits = 2)



# -->
# in general, more reliable than the usual standard errors when the variance model is suspected not to be correct
# (assuming the mean model is correct).
# The sandwich standard errors are usually (but not always) larger than the usual ones



# ------------------------------------------------------------------------------
# orthogonal polynomials using poly()
# ------------------------------------------------------------------------------

m0 <- gamlss(lborev1 ~ poly(lboopen, 3), data = film90, family = NO)

summary(m0)

lines(fitted(m0)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "blue")



# ------------------------------------------------------------------------------
# Compare the correlations between the parameter estimates for m00 and m0
# ------------------------------------------------------------------------------

library(corrplot)

col1 <- colorRampPalette(c("black", "grey"))

corrplot(vcov(m00, type = "cor"), col = col1(2), outline = TRUE, tl.col = "black", addCoef.col = "white")

corrplot(vcov(m0, type = "cor"), col = col1(2), outline = TRUE, tl.col = "black", addCoef.col = "white")


# -->
# the parameter of the mu model for m0 are uncorrelated because we used orthogonal polynomials
# but for m00 they are highly correlated.



