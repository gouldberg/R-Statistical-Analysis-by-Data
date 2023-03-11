# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# -->
# Continued from previous scripts

# ------------------------------------------------------------------------------
# Cointegration test for federal funds rate and bond rate by error correction model
#    - The error correction model can also be used to test the two time series for cointegration
# ------------------------------------------------------------------------------
ehat <- bfset$Lb - coef(bf.nls)[[2]] - coef(bf.nls)[[3]] * bfset$Lf  # estimated errors

ehat <- ts(ehat)

ehat.adf <- dynlm(d(ehat) ~ L(ehat, 1) + L(d(ehat), 1) + L(d(ehat), 2) - 1)

kable(tidy(ehat.adf), caption = "Stationarity test Within the Error Correction Model")


# -->
# The relevant statistis is -5.559  --> critical value is -3.37, cannot reject the H0 of non-cointegration
# The result is to reject the null of no cointegration
