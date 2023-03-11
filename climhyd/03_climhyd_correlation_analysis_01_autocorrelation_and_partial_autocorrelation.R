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
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))



# ----------
# Temperature
plot(climhyd$Temp, type = "l", main = "Temp")
astsa::acf2(climhyd$Temp, max.lag = 100, main = "Temp")



# ----------
# DewPt
plot(climhyd$DewPt, type = "l", main = "DewPt")
astsa::acf2(climhyd$DewPt, max.lag = 100, main = "DewPt")



# ----------
# Cloud Cover
plot(climhyd$CldCvr, type = "l", main = "Cloud Cover")
astsa::acf2(climhyd$CldCvr, max.lag = 100, main = "Cloud Cover")



# ----------
# Wind Speed
plot(climhyd$WndSpd, type = "l", main = "Wind Speed")
astsa::acf2(climhyd$WndSpd, max.lag = 100, main = "Wind Speed")



# ----------
# Logged Inflow
plot(inf, type = "l", main = "logged Inflow")
astsa::acf2(inf, max.lag = 100, main = "logged Inflow")

plot(climhyd$Inflow, type = "l", main = "Inflow")
astsa::acf2(climhyd$Inflow, max.lag = 100, main = "Inflow")



# ----------
# Sqrt Precipitation
plot(prec, type = "l", main = "sqrt Precipitation")
astsa::acf2(prec, max.lag = 100, main = "sqrt Precipitation")

plot(climhyd$Precip, type = "l", main = "Precipitation")
astsa::acf2(climhyd$Precip, max.lag = 100, main = "Precipitation")
