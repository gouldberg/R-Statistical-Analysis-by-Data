setwd("//media//kswada//MyFiles//R//brain")

packages <- c("dplyr", "lattice", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  brain
# ------------------------------------------------------------------------------
data(brain, package = "gamair")

str(brain)



# 2 voxels appear problematic, these voxels have medFPQ values recorded as 3 * 10^-6 and 4 * 10^-7, while the remaining 1565 voxels have values in the range 0.003 to 20.
# Residual plots from all attempts to model the data set including these two voxels consistently show them as grotesque outliers.

# exclude 2 outliers.
brain <- brain[brain$medFPQ > 5e-3,]



# ------------------------------------------------------------------------------
# fitted values on the scale of the linear predictor and on the response scale
# ------------------------------------------------------------------------------

# firstly on the scale of the linear predictor
predict(m2)[1:5]

pv <- predict(m2, se = TRUE)

pv$fit[1:5]
pv$se[1:5]



# ----------
# secondary on the response scale
predict(m2, type = "response")[1:5]

pv <- predict(m2, type = "response", se = TRUE)

pv$fit[1:5]
pv$se[1:5]


# Note that the standard errors provided on the response scale are approximate, being obtained by the usual Taylor expansion approach.



# ------------------------------------------------------------------------------
# predictions for newdata
# ------------------------------------------------------------------------------

pd <- data.frame(X = c(80.1, 68.3), Y = c(41.8, 41.8))

m2

predict(m2, newdata = pd)

predict(m2, newdata = pd, type = "response", se = TRUE)



# ------------------------------------------------------------------------------
# Obtain the contributions that each model term, excluding the intercept, makes to the linear predictor
# ------------------------------------------------------------------------------

summary(m3)

predict(m3, newdata = pd, type = "terms", se = TRUE)



# ------------------------------------------------------------------------------
# Obtain prediction matrix (Xp), which maps the model parameters beta to the predictions of the linear predictor = Xp * beta
# predict.gam can return Xp ("lpmatrix")
#
# --> A major use of Xp is in the calculation of variances for combinations of linear predictor values
# ------------------------------------------------------------------------------

Xp <- predict(m2, newdata = pd, type = "lpmatrix")

Xp


# -->
# The returned matrix is of dimension 2 * 100 (with default treatment of identifiability constraints)

fv <- Xp %*% coef(m2)

fv


