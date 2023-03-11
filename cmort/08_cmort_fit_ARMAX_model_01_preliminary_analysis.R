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
# Preliminary analysis for ARMAX model:  cmort
# ------------------------------------------------------------------------------


# ACF and PACF for detrended cmort
cmort_d <- resid(lm(cmort ~ time(cmort)))


astsa::acf2(cmort_d, max.lag = 96)



# ----------
# Also check by sarima
fit1 <- sarima(cmort, 2, 0, 0, xreg = time(cmort))


fit1$ttable



# -->
# An AR(2) model fits well to detrended cmort



# ------------------------------------------------------------------------------
# Preliminary analysis for ARMAX model:  tempr
# ------------------------------------------------------------------------------


# ACF and PACF for detrended tempr
tempr_d <- resid(lm(tempr ~ time(tempr)))


astsa::acf2(tempr_d, max.lag = 96)



# ----------
# Also check by sarima
fit1_2 <- sarima(tempr, 4, 0, 0, xreg = time(tempr))


fit1_2$ttable



# -->
# temperature does NOT fit AR model



# ------------------------------------------------------------------------------
# Preliminary analysis for ARMAX model:  other variables
# ------------------------------------------------------------------------------

# CCF between the mortality residuals, the temperature series and the particulates series

dmort <- resid(fit1$fit)

dtempr <- resid(fit1_2$fit)


acf(cbind(dmort, tempr, part), cex.main = 2)

# acf(cbind(dmort, dtempr, part), cex.main = 2)



# -->
# #### note that top 2nd and 3rd lag acf data 
# temperature:  leading 1 week to dmort


# #### note that top 3nd and 4rd lag acf data 
# particulate:  leading 4 weeks to dmort




# ----------
lag2.plot(tempr, dmort, 1)

lag2.plot(part, dmort, 4)



# -->
# From these results, we decided to fit the ARMAX model

# demort = dmort AR(2) + tempr lag1 + part lag4

# detrended_mortality(t) = 
# phi1 * detrended_mortality(t-1) + phi2 * detrended_mortality(t-2) + beta1 * temperature(t-1) + beta2 * particualte + beta3 * particulate(t-4) + v(t)


