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
# data exploration:  monthplot
# ------------------------------------------------------------------------------

Temp <- ts(climhyd$Temp, start = 1, frequency = 12)

DewPt <- ts(climhyd$DewPt, start = 1, frequency = 12)

cldcvr <- ts(climhyd$CldCvr, start = 1, frequency = 12)

WndSpd <- ts(climhyd$WndSpd, start = 1, frequency = 12)

prec <- ts(prec, start = 1, frequency = 12)

inf <- ts(inf, start = 1, frequency = 12)



# ----------
graphics.off()
par(mfrow = c(2,3))

monthplot(Temp)

monthplot(DewPt)

monthplot(cldcvr)

monthplot(WndSpd)

monthplot(prec)

monthplot(inf)



