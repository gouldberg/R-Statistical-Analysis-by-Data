setwd("//media//kswada//MyFiles//R//aids")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aids
# ------------------------------------------------------------------------------
data("aids", package = "gamlss.data")


str(aids)

car::some(aids)



# ------------------------------------------------------------------------------
# The gen.likelihood() funtion
#   - Th function gen.likelihood() generates the likelihood function of the model for the purpose of creating the Hessian matrix
#     required for construction of the standard errors of the parameter estimates.
#   - This function is used by vcov() to obtain the correct Hessian matrix, after a model has been fitted.
#     This is automatically used by summary() to obtain standard errors of the parameter estimates.
# ------------------------------------------------------------------------------

m100 <- gamlss(y ~ x + qrt, data = aids, family = NBI)



# ----------
# generate the log-likelihood function
logL <- gen.likelihood(m100)


# evaluate it at the final fitted values
logL()


# This code is equivalent
logL(c(coef(m100), coef(m100, "sigma")))



# ----------
# The Hessian
optimHess(c(coef(m100), coef(m100, "sigma")), logL)



# -->
# Note that the 1st intercept is in the predictor model for mu, while the 2nd intercept is in the predictor model for sigma
# in the negative binomial NBI(mu, sigma) distribution.



# ----------
m200 <- gamlss(y ~ pb(x) + qrt, data = aids, family = NBI)

logL2 <- gen.likelihood(m200)
logL2(c(coef(m200), coef(m200, "sigma")))

optimHess(c(coef(m200), coef(m200, "sigma")), logL)


# -->
# When smoothing terms are fitted, gen.likelihood() considers them as fixed at their fitted values, sot the Hessian in this case
# does not take into account the variability in the fitting of the smoothers.



# ------------------------------------------------------------------------------
# Correlation and standard errors
# ------------------------------------------------------------------------------

# correlation between the fitted parameters
vcov(m100, type = "cor")



# ----------
# standard errors
vcov(m100, type = "se")



# ----------
# the sandwich standard errors
vcov(m100, type = "se", robust = TRUE)

# this is the same
rvcov(m100, type = "se")



# ------------------------------------------------------------------------------
# Robust standard errors
#   - Robust standard errors may be more reliable than the usual standard errors when the variance model is suspected NOT to be correct
#     (assuming the mean model is correct).
#   - The sandwich standard errors are usually (but not always) larger than the usual ones.
# ------------------------------------------------------------------------------

set.seed(4321)


# ----------
# generate from a gamma distribution with sigma = 2
Y <- rGA(200, mu = 1, sigma = 2)



# ----------
# fitting the wrong model i.e. sigma = 1
r1 <- gamlss(Y ~ 1, family = EXP)


# fitting correct model
r2 <- gamlss(Y ~ 1, family = GA)



# ----------
vcov(r1, type = "se")
rvcov(r1, type = "se")


# -->
# the conventioanl se is too precise



# ----------
vcov(r2, type = "se")
rvcov(r2, type = "se")
vcov(r2, type = "se", robust = TRUE)

