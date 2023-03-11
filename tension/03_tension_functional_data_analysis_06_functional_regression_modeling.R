setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension



# ------------------------------------------------------------------------------
# Functional Regression:  How predictors behave along the time continuum.
#   - refund package is designed for advanced regression modeling on functional data
# ------------------------------------------------------------------------------

library(refund)


# ----------
# convert the data into an fd object
ftension2 <- fdata2fd(ftension1)


ftension2$basis


ftension2$coefs




# ---------
graphics.off()

par(mfrow = c(1,1))

plot(ftension2)


# same
# plot(ftensionNP$fdata.est, main = "Smooth Data")




# ----------
# set up the design matrix
# auditory as baseline

X <- model.matrix(~ cond)  ## auditory as baseline


head(X,20)





# ------------------------------------------------------------------------------
# Fit the functional regression model
# Functional Regression:  How predictors behave along the time continuum.
# ------------------------------------------------------------------------------

# Note that instead of obtaining a single value for each regression parameter as in ordinary regression,
# in functional regression each regression parameter is a function along time, potentially subject to further smoothing.
# Here, mostly for running time purposes, we set the smoothing parameter to a single value of lambda = 100 which works well in this example.
# A good, albeit time-consuming option is to let the algorithm pick the optimal lambda by means of CV (default in the function below)


# X: scalar predictors
tenreg <- fosr(fdobj = ftension2, X = X, lambda = 100)

tenreg



# ----------
refund:::plot.fosr(tenreg, titles = c("Intercept", "Visual vs Auditory", "Visual-Auditory vs. Auditory"))



# -->
# We can see that in certain segments along the time continuum (i.e., within first 20 and 40 - 60s, approximately)
# the effect of auditory vs. vidual differs significantly from 0.

# There is no significant difference at any point in time between autitory and visual-auditory condition


