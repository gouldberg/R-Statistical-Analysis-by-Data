setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(2,1))

plot(lynx, type = "o")

plot(diff(lynx), type = "o")



# ----------
# log10 base

lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))


par(mfrow = c(2,1))

plot(lynxl, type = "o")
lines(smooth.spline(time(lynxl), lynxl, spar = 1), col = "blue", lty = 1)

plot(dlynxl, type = "o")
lines(smooth.spline(time(dlynxl), dlynxl, spar = 1), col = "blue", lty = 1)


