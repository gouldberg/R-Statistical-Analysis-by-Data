setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

library(tsDyn)


dflu <- diff(flu)



# ----------
# Self Threshold Autoregressive model
# m:  embedding dimensions
# d:  time delay
# steps:  forecasting steps
# thDelay:  time delay for the threshold variable (as multiple of embedding time delay d)
# th:  threshold value (if missing, a search over a reasonable grid is tried)


u <- setar(dflu, m = 4, thDelay = 0, th = .05)




# ----------
graphics.off()

par(mfrow = c(2,2))

plot(u)




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn:  automatic selection of threshold
# ------------------------------------------------------------------------------

# let program fit threshold
u2 <- setar(dflu, m = 4, thDelay = 0)


u2$coefficients


# -->
# threshold = 0.036


# ----------
plot(u2)
# ?plot.setar




# ------------------------------------------------------------------------------
# Try m = 3:  works well too
# ------------------------------------------------------------------------------

u3 <- setar(dflu, m = 3, thDelay = 0)


u3$coefficients


plot(u3)



# ----------
BIC(u) 
BIC(u2) 
BIC(u3) 



