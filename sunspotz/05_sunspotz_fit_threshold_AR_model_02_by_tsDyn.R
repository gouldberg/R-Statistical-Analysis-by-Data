setwd("//media//kswada//MyFiles//R//sunspotz")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sunspotz
# ------------------------------------------------------------------------------

data(sunspotz, package = "astsa")

str(sunspotz)

head(sunspotz)



# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

library(tsDyn)


dsun <- diff(sun)



# ----------
# Self Threshold Autoregressive model
# m:  embedding dimensions
# thDelay:  time delay for the threshold variable (as multiple of embedding time delay d)
# th:  threshold value (if missing, a search over a reasonable grid is tried)
u <- setar(dsun, m = 10, thDelay = 0, th = 4.0)


plot(u)




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA) by tsDyn:  automatic selection of threshold
# ------------------------------------------------------------------------------

# let program fit threshold
u2 <- setar(dsun, m = 4, thDelay = 0)


u2$coefficients


# -->
# threshold = 5.0


# ----------
plot(u2)
# ?plot.setar



BIC(u) 
BIC(u2) 



