# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/phillips5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  phillips5_aus
# ------------------------------------------------------------------------------
data("phillips5_aus", package = "POE5Rdata")

data <- phillips5_aus

glimpse(data)

str(data)

dim(data)


# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,3,4)], start = c(1987, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# OLS estimate of the Phillips curve equation
#   - inf(t) = alpha + beta0 + D(u(t)) + e(t)
# ------------------------------------------------------------------------------
phill.dyn <- dynlm(inf ~ diff(u), data = data.ts)


kable(tidy(phill.dyn), digits=3, caption = "Summary of the Phillips Model")



# ------------------------------------------------------------------------------
# Estimation with AR(1) errors
#   - e(t) = rho * e(t-1) + v(t)
#   - AR(1) model for the error term is assumed to have zero mean and constant variance, and its errors should not be autocorrelated.
# ------------------------------------------------------------------------------
tmp <- dynlm(residuals(phill.dyn) ~ diff(residuals(phill.dyn)))

summary(tmp)


# -->
# The correlation coefficient fo the first lag is 0.50, some correlation.



# ----------
graphics.off()
par(mfrow = c(1, 2))
plot(residuals(phill.dyn))
plot(residuals(tmp))



