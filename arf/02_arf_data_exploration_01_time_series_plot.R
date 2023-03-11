setwd("//media//kswada//MyFiles//R//arf")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  arf
# ------------------------------------------------------------------------------

data(arf, package = "astsa")

str(arf)

head(arf)



# ------------------------------------------------------------------------------
# data exploration:  align time series and multivarite time series plot
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,1))
plot(arf)
plot(diff(arf))



# ----------
forecast::ndiffs(arf)
