setwd("//media//kswada//MyFiles//R//motorins")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  motorins
# ------------------------------------------------------------------------------

data(motorins, package="faraway")

str(motorins)


# attention has been restricted to data from Zone 1

motori <- motorins[motorins$Zone == 1,]



# ------------------------------------------------------------------------------
# Additive model by gam package
#   - gam package originates from the work of Hastie and Tibshirani (1900)
#     The gam package allows more choice in the smoothers used
#     while the mgcv package has an automatic choice in the amount of smoothing as well as wider functionality
#   - gam packages use backfitting algorithm for fitting.
#     The algorithm is iterated until convergence. Hastie and Tibshirani show that convergence is assured under some rather loose conditions.
# ------------------------------------------------------------------------------

library(gam)


# ----------
# Gaussian additive model with a logged response
amgam_log <- gam(log(Payment) ~ offset(log(Insured)) + lo(as.numeric(Kilometres)) + lo(Make) + lo(Bonus), data = motori, trace=TRUE)

summary(amgam_log)


# Deviance expalined = 1 - Residual Deviance / Null Deviance
1 - amgam_log$deviance / amgam_log$null.deviance



# ----------
# gamma GAM for the unstransformed response
amgam_gamma <- gam(Payment ~ offset(log(Insured)) + lo(as.numeric(Kilometres)) + lo(Make) + lo(Bonus), family = Gamma(link=log), data = motori, trace=TRUE)

summary(amgam_gamma)

# Deviance expalined = 1 - Residual Deviance / Null Deviance
1 - amgam_gamma$deviance / amgam_gamma$null.deviance
1 - gl$deviance / gl$null.deviance



# -->
# gamma GAM is almost same with gamma GLM
# gamma GAM is better than Gaussian additive model with logged response
summary(gl)
summary(amgam_gamma)
summary(amgam_log)




# ------------------------------------------------------------------------------
# Additive model by mgcv package  --> PRODUCES ERRORS !!!
#   - mgcv package is part of the recommended suite that comes with the default installation of R and is based on methods described in Wood (2000).
#   - mgcv package has an automatic choice in the amount of smoothing as well as wider functionality.
#     but splines are only choice of smoother, but the appropriate amount of smoothing is internally chose by default.
#   - mgcv package employs a penalized smoothing spline approach and likelihood approach.
#     GCV is used to select lambda (controlling the amount of smoothing for each variable)
# ------------------------------------------------------------------------------

library(mgcv)


# Gaussian additive model with a logged response
amint_log <- mgcv::gam(log(Payment) ~ offset(log(Insured)) + s(as.numeric(Kilometres)) + s(Make) + s(Bonus), data = motori, trace=TRUE)

# --> this will produce errors


# ----------
# gamma GAM for the unstransformed response
ammgcv_gamma <- mgcv::gam(Payment ~ offset(log(Insured)) + s(as.numeric(Kilometres)) + s(Make) + s(Bonus), family = Gamma(link=log), data = motori, trace=TRUE)

# --> this will produce errors


