setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")

str(sp500w)

sp500w



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,1))
plot(sp500w, type = "l")
plot(sp500w, type = "h")



# ----------
forecast::ndiffs(sp500w)

