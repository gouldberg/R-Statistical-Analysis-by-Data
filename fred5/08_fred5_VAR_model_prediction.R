# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/fred5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# --> Continued from previous scripts

# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

# Out-of-sample forecasting of VAR models and fan-chart
var.pred <- predict(varfit, n.ahead = 10, ci = 0.95)
plot(var.pred)


fanchart(var.pred)


# VARpred
VARpred(varfit_MTS_ref, h = 10)

