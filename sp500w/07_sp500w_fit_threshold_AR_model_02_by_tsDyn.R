setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)


# convert to ts object
sp500w_c <- ts(sp500w, start = 2003, freq = 52)




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

library(tsDyn)


# ----------
# Self Threshold Autoregressive model
# m:  embedding dimensions
# thDelay:  time delay for the threshold variable (as multiple of embedding time delay d)
# th:  threshold value (if missing, a search over a reasonable grid is tried)

u <- setar(sp500w_c, m = 4, thDelay = 0, th = -0.01)



par(mfrow = c(3,3))
plot(u)





# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn:  automatic selection of threshold
# ------------------------------------------------------------------------------

# let program fit threshold
u2 <- setar(sp500w_c, m = 4, thDelay = 0)


u2$coefficients


# -->
# threshold = 5 ...?


# ----------
plot(u2)
# ?plot.setar



# ----------
BIC(u) 
BIC(u2) 





# ------------------------------------------------------------------------------
# Automatic seleciton of SETAR hyerparameters
# ------------------------------------------------------------------------------


# IT TAKES TIME !!!:  3 min
# m = 10:  using maximum autoregressive order for low regime mL = 10 and high regime mH = 10

selectSETAR(sp500w_c, m = 10)

selectSETAR(sp500w_c, m = 5, nthresh = 2)



# ----------
# fit by best parameters

# u3 <- setar(sp500w_c, thDelay = 0, mL = 3, mH = 2, th = -0.01947625)

u3 <- setar(sp500w_c, thDelay = 0, nthresh = 2, mL = 3, mH = 3, mM = 3)



u3$coefficients




# ----------
plot(u3)




