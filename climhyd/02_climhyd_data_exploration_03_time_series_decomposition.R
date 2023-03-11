setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)



# ------------------------------------------------------------------------------
# data exploration:  time series decomposition by stats::decompose
# ------------------------------------------------------------------------------

Temp <- ts(climhyd$Temp, start = 1, frequency = 12)

DewPt <- ts(climhyd$DewPt, start = 1, frequency = 12)

WndSpd <- ts(climhyd$WndSpd, start = 1, frequency = 12)

cldcvr <- ts(climhyd$CldCvr, start = 1, frequency = 12)

prec <- ts(prec, start = 1, frequency = 12)

inf <- ts(inf, start = 1, frequency = 12)



# ----------
graphics.off()
par(mfrow = c(1,1))

plot(decompose(Temp, type = "multiplicative"))

plot(decompose(DewPt, type = "multiplicative"))

plot(decompose(cldcvr, type = "multiplicative"))

plot(decompose(WndSpd, type = "multiplicative"))

plot(decompose(prec, type = "multiplicative"))

plot(decompose(inf, type = "multiplicative"))




# ----------
plot(decompose(Temp, type = "additive"))

plot(decompose(DewPt, type = "additive"))

plot(decompose(cldcvr, type = "additive"))

plot(decompose(WndSpd, type = "additive"))

plot(decompose(prec, type = "additive"))

plot(decompose(inf, type = "additive"))



