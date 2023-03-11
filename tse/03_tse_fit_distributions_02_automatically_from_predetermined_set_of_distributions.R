setwd("//media//kswada//MyFiles//R//tse")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tse
#   - Histogram of the Turkish stock exchange returns data
# ------------------------------------------------------------------------------
data("tse", package = "gamlss.data")


str(tse)

car::some(tse)



# ------------------------------------------------------------------------------
# Fit a set of predetermined distributions to the data and choose the best
#   - fitDist() uses gamlssML() to fit a set of predetermined distributions to the data
#     and choose the best according to the GAIC, with default penalty k = 2
# ------------------------------------------------------------------------------

# All of the gamlss.family distributions on the real line have been considered
# realline: continuous distributions on (-inf < y < inf)
m5 <- fitDist(ret, data = tse, type = "realline", k = 2)



# ----------
m5



# ----------
# shows GAIC from best model to worst model
m5$fits



# -->
# The most prefered distribution is the skew exponential power type 2 (SEP2), and
# the least prefered is the reverse Gumbel (RG)


# ----------
# No fails
m5$fails




# ----------
# Repeat with k = 3.84 and k = log(length(tse$ret)) corresponding to criteria X^2(1, 0.05) and Schwartz Bayesian Criteria (SBC), respectively
m6 <- fitDist(ret, data = tse, type = "realline", k = 3.84)

m7 <- fitDist(ret, data = tse, type = "realline", k = log(length(tse$ret)))

m6$fits
m6$fails

m7$fits
m7$fails



# ----------
# plot the best SEP2 model
histDist(ret, data = tse, family = "SEP2", nbins = 30, line.wd = 2.5)



# ----------
# refit the model using gamlss() in order to output the parameter estimates using summary() 
m5_2 <- gamlss(ret ~ 1, data = tse, family = "SEP2")

summary(m5_2)

