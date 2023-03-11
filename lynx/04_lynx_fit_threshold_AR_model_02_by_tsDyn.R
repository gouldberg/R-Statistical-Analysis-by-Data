setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ----------
lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn:  automatic selection of threshold
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

library(tsDyn)


# ----------
# Self Threshold Autoregressive model
# m:  embedding dimensions
# d:  time delay
# steps:  forecasting steps
# thDelay:  time delay for the threshold variable (as multiple of embedding time delay d)
# th:  threshold value (if missing, a search over a reasonable grid is tried)



# both of low regime and high regime are AR(3)  (L = H = 3)

u <- setar(lynxl, m = 3)


summary(u)



# -->
# but suggesting
# low regime:  AR(1)
# high regime:  AR(2)




u$coefficients


# -->
# threshold = 2.5877



# ----------
plot(u)
# ?plot.setar





# ------------------------------------------------------------------------------
# Different AR order each for low regime and high regime
# ------------------------------------------------------------------------------


u2 <- setar(lynxl, mL = 1, mH = 2)


summary(u2)



# -->
# threshold = 2.558




# ------------------------------------------------------------------------------
# applied state: t - 2
# ------------------------------------------------------------------------------


# if t - 2

u3 <- setar(lynxl, mL = 1, mH = 2, thDelay = 1)


summary(u3)



# -->
# threshold = 2.867




# ------------------------------------------------------------------------------
# number of states = 3
# ------------------------------------------------------------------------------


# low regime AR(1),  middle regime AR(2) and high regime AR(3)
# number of thresholds = 2

u4 <- setar(lynxl, mL = 1, mM = 2, mH = 3, thDelay = 1, nthresh = 2)


summary(u4)




# ----------
# excluding non significant AR term

u5 <- setar(lynxl, mL = 1, mM = 1, mH = 2, thDelay = 1, nthresh = 2)


summary(u5)




# ------------------------------------------------------------------------------
# exogeneous variable
# ------------------------------------------------------------------------------

Time <- 1:114


u6 <- setar(lynxl, mL = 1, mH = 2, thVar = Time)


summary(u6)



# -->
# structural change occurs at 17th time points


plot(lynxl, type = "l")

abline(v = 1820 + 17)




# ------------------------------------------------------------------------------
# Logistic Smooth Transition AutoRegression model (lstar)
# ------------------------------------------------------------------------------


# all regimes should have same AR order for lstar

lu <- lstar(lynxl, m = 2)


summary(lu)




# ------------------------------------------------------------------------------
# model selection
# ------------------------------------------------------------------------------


selectSETAR(lynxl, m = 3, mL = 1:3, mH = 1:3, thSteps = 5, thDelay = 0:2)



# -->
# thDelay = 2, mL = 1, mH = 1 is best
# the threshold = 2.94



# ----------
u7 <- setar(lynxl, mL = 1, mH = 1, thDelay = 2)


summary(u7)




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------


BIC(u)
BIC(u2)
BIC(u3)
BIC(u4)
BIC(u5)
BIC(u6)
BIC(u7)
BIC(lu)




# ------------------------------------------------------------------------------
# plot model  (especially model skeleton)
# ------------------------------------------------------------------------------


plot(u3)




# ----------
# model skeleton

lag.plot(predict(u2, n.ahead = 100), 1)



# applied state: t - 2
lag.plot(predict(u3, n.ahead = 100), 1)

