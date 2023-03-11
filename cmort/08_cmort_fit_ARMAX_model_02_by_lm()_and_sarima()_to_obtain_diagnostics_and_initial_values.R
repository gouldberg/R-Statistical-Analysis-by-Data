setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Preparation
#   - Some additional data analysis notes are:
#       - (1) Time is centered, in this case, intercept used in detrending should be close to the average value of mortality
#       - (2) Particulate(t) and Particulate(t-4) are highly correlated, so orthogonalizing these two inputs would be advantageous (altough we did not do it here,
#         perhaps by partialling out Particulate(t-4) from Particulate(t) using simple linear regression
#       - (3) Temperature(t) and Temperature(t)^2 are not needed in the model when Temperature(t-1) is included
#       - (4) Initial values of the parameters are taken from a preliminary investigation
# ------------------------------------------------------------------------------

# center time
trend <- time(cmort) - mean(time(cmort))


dcmort <- resid(fit2 <- lm(cmort ~ trend, na.action = NULL))



# ----------
# intercept = 88.7,  trend = -1.63
fit2



# ----------
u <- ts.intersect(dM = dcmort, dM1 = stats::lag(dcmort, -1), 
                  dM2 = stats::lag(dcmort, -2), T1 = stats::lag(tempr, -1),
                  P = part, P4 = stats::lag(part, -4))




# ------------------------------------------------------------------------------
# Fit ARMAX model by lm() and check diagnostics
# ------------------------------------------------------------------------------

mod_lm <- lm(dM ~ ., data = u, na.action = NULL)


summary(mod_lm)


par(mfrow = c(2,2))
plot(mod_lm)




# ------------------------------------------------------------------------------
# Use sarima to provide a thorough anaysis of residuals
# ------------------------------------------------------------------------------

# get residual analysis as a byproduct
sarima(u[,1], 0, 0, 0, xreg = u[,2:6])



# -->
# The residual analysis supports the model


