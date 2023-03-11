setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



# ------------------------------------------------------------------------------
# Residual diagnostics:  Z and Q statistics
#   - test normality of the residuals within ranges of an independent x-variable
#   - The statistics Z1, Z2, Z3, Z4 are calculated from the residuals in group g have population mean 0, variance 1, moment-based skewness 0 and moment-based kurtosis 3
#   - Q statistics:  Q(j) = sum(Z(gj)^2)  j = 1,2,3,4
#   - Rough guide values of abs(Z(gj)) > 2 be considered as indicative of significant inadequacies in the model
#     Significant positive (or negative values) for g = 1,2,3 or 4 indicate, respectively, that the redisulas have a higher (lr lower) mean, variance, skewness or kurtosis
#     than the standard normal distribution.
#     --> The model for parameter mu, sigma, nu or tau may need more degrees of freedom to over come this.
# ------------------------------------------------------------------------------


par(mfrow=c(1,1))

m0_c <- lms(head, age, families = c("BCCGo", "BCPEo", "BCTo"), calibration = F, data = db, trans.x = TRUE, k = 4)



# ----------
round(Q.stats(m0_c, xvar = db$age, n.inter = 20), 3)



# ------------------------------------------------------------------------------
# Residual diagnostics:  multiple worm plots
# ------------------------------------------------------------------------------

wp(m0_c, ylim.all = 1)

wp(m0_c, xvar = db$age, ylim.worm = 1.5, n.inter = 16)



