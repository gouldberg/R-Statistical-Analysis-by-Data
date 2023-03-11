# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gdp
#  - GDP series for Australia and USA for the period 1970Q1 to 2000Q4
# ------------------------------------------------------------------------------
data("gdp", package = "POE5Rdata")


glimpse(gdp)
str(gdp)


# ----------
# convert to ts object
is.ts(gdp)

gdp <- ts(gdp, start = c(1970, 1), end = c(2000, 4), frequency = 4)



# --> continued from previous scripts

# ------------------------------------------------------------------------------
# Vector Error Correction Model (VEC model)
#   - If the two variables in two vector autoregression models are cointegrated, their relationship should be taken into account in the model.
#     y(t) = beta10 + beta11 * y(t-1) + beta12 * x(t-1) + vt(y)
#     x(t) = beta20 + beta21 * y(t-1) + beta22 * x(t-1) + vt(x)
#
#   - The estimation method
#      - First, estimate the cointegrating relationship:  y(t) = beta0 + beta1 * x(t) + e(t)
#      - Create the lagged residual series:  e(t-1) = y(t-1) - b0 - b1 * x(t-1)
#      - Estimate equations by OLS
#          - d(y(t)) = alpha10 + alpa11 + e(t-1) + vt(y)
#          - d(x(t)) = alpha20 + alpa21 + e(t-1) + vt(x)
# ------------------------------------------------------------------------------

# With cointegrated series we can construct a VEC model to better understand the causal relationship between the two variables

vecaus <- dynlm(d(aus) ~ L(ehat), data = gdp)

vecusa <- dynlm(d(usa) ~ L(ehat), data = gdp)



# ----------
tidy(vecaus)


# -->
# The coefs on the error correction term L(ehat) is significant for Australia, suggesting that changes in the US economy do affect Australian economy



# ----------
tidy(vecusa)


# -->
# The coefs on the error correction term L(ehat) is NOT significant for USA equation, suggesting that changes in the AUS economy do NOT affect USA economy



# ----------
# y(t) = beta0 + beta1 * x(t) + e(t)
kable(tidy(cint1.dyn, digits = 3, caption = "The Results of the Cointegration Equation"))


# -->
# To interpret the sign of the error correction coefficient, one should remember that e(t-1) measures the deviation of Australlian economy
# from its cointegrating level of 0.985 (= beta1) of the US economy
