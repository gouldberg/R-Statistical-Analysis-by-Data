setwd("//media//kswada//MyFiles//R//eustockmarkets")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EuStockMarkets
# ------------------------------------------------------------------------------
data("EuStockMarkets", package = "datasets")


str(EuStockMarkets)

head(EuStockMarkets)



# ----------
dax <- EuStockMarkets[, "DAX"]

Rdax <- diff(log(dax))




# ------------------------------------------------------------------------------
# Fit a set of predetermined distributions to the data and choose the best
#   - fitDist() uses gamlssML() to fit a set of predetermined distributions to the data
#     and choose the best according to the GAIC, with default penalty k = 2
# ------------------------------------------------------------------------------

library(gamlss)


# All of the gamlss.family distributions on the real line have been considered
# realline: continuous distributions on (-inf < y < inf)

f1 <- fitDist(Rdax, type = "realline", k = 2, trace = TRUE)



# -->
# Several warnings may appear. Most of the time these have to do with convergence problems that occurred during the fitting of specific distributions.
# If a distribution is not appropriate for the data set, the fact that it failed to converge is of no relevance,
# and in this case the warnings can be safely ignored.
# However occasionally distributions converge to a local maximum of the likelihood function.



# ----------
f1


# ----------
# shows GAIC from best model to worst model
f1$fits



# -->
# The best fitted distribution using the AIC for the DAX returns is the 4-parameter generalized t, GT
# In GT, nu and tau model different aspects of the kurtosis of the distribution.

# Note that the two power exponential distributions PE2 and PE have identical GAIC.
# This is the result of fitting reparameterized distributions when no explanatory variables are used.
# The fitted distributions in these cases are identical (and therefore have identical deviances),
# but the fitted parameters and their interpretations differ.



# ----------
# No fails
f1$fails




# ----------
# Repeat with k = 3.84 and k = log(length(Rdax)) corresponding to criteria X^2(1, 0.05) and Schwartz Bayesian Criteria (SBC), respectively
f2 <- fitDist(Rdax, type = "realline", k = 3.84, trace = TRUE)

f3 <- fitDist(Rdax, type = "realline", k = log(length(Rdax)), trace = TRUE)

f2$fits
f2$fails

f3$fits
f3$fails



# ----------
# plot the best GT model
histDist(Rdax, family = "GT", nbins = 30, line.wd = 2.5)




# ----------
# refit the model using gamlss() in order to output the parameter estimates using summary() 

f1_2 <- gamlss(Rdax ~ 1, family = "GT")

summary(f1_2)

