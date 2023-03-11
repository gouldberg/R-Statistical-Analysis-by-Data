setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

library(tsDyn)


deqc <- diff(EQcount)



# ----------
# Self Threshold Autoregressive model
# m:  embedding dimensions
# thDelay:  time delay for the threshold variable (as multiple of embedding time delay d)
# th:  threshold value (if missing, a search over a reasonable grid is tried)

u <- setar(deqc, m = 4, thDelay = 0, th = -3)



par(mfrow = c(3,3))
plot(u)




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn:  automatic selection of threshold
# ------------------------------------------------------------------------------

# let program fit threshold
u2 <- setar(deqc, m = 4, thDelay = 0)


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

selectSETAR(deqc, m = 10)

selectSETAR(deqc, m = 5, nthresh = 2)



# ----------
# fit by best parameters

u3 <- setar(deqc, thDelay = 0, nthresh = 2, mL = 5, mH = 5, mM = 5)


u3$coefficients




# ----------
plot(u3)

  