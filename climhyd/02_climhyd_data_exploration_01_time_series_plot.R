setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)



# ------------------------------------------------------------------------------
# data exploration:  multivarite time series plot
# ------------------------------------------------------------------------------

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))

MTS::MTSplot(climhyd)



# -->
# from top-left to bottom-left:  Temp, DewPt, CldCvr
# from top-right to bottom-right:  WndSpd, Precip, Inflow

# Inflow is strongly related to precipitation


# -->
# But precipitation has many zero values ... difficult to analyze by correlation analysis




# ----------
graphics.off()

cyc <- 12

par(mfrow=c(4,1), mar = c(2,2,2,2))

plot(climhyd$Temp, ylab = "", xlab = "", type = "l", main = "Temp")
abline(v = seq(1, 444, by = cyc), lty = 2, col = gray(0.4))

plot(climhyd$WndSpd, ylab = "", xlab = "", type = "l", main = "WndSpd")
abline(v = seq(1, 444, by = cyc), lty = 2, col = gray(0.4))

plot(climhyd$Precip, ylab = "", xlab = "", type = "l", main = "Precip")
abline(v = seq(1, 444, by = cyc), lty = 2, col = gray(0.4))

plot(climhyd$Inflow, ylab = "", xlab = "", type = "l", main = "Inflow")
abline(v = seq(1, 444, by = cyc), lty = 2, col = gray(0.4))



# -->
# Temperature, Precipitation, and Inflow (not for Wind Speed)
# has somewhat 12 months cycle




# ----------
graphics.off()

cyc <- 20

par(mfrow=c(4,1), mar = c(2,2,2,2))

plot(climhyd$Temp, ylab = "", xlab = "", type = "l", main = "Temp")
abline(v = seq(1, 440, by = cyc), lty = 2, col = gray(0.4))

plot(climhyd$WndSpd, ylab = "", xlab = "", type = "l", main = "WndSpd")
abline(v = seq(1, 440, by = cyc), lty = 2, col = gray(0.4))

plot(climhyd$Precip, ylab = "", xlab = "", type = "l", main = "Precip")
abline(v = seq(1, 444, by = cyc), lty = 2, col = gray(0.4))

plot(climhyd$Inflow, ylab = "", xlab = "", type = "l", main = "Inflow")
abline(v = seq(1, 440, by = cyc), lty = 2, col = gray(0.4))



# -->
# But also 20 months cycle seems to be strong, too.


