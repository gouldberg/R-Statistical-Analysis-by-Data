setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

graphics.off()

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))

MTS::MTSplot(airquality[,c("Ozone", "Solar.R", "Wind", "Temp")])




# ------------------------------------------------------------------------------
# data exploration:  smoothing for Wind
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


tdat <- airquality$Wind


plot(tdat, type = "l")

lines(smooth.spline(time(tdat), tdat, spar = 0.5), lwd = 2, col = "blue")

lines(smooth.spline(time(tdat), tdat, spar = 0.75), lwd = 2, col = "red")

abline(v = c(30 * 1:5), lty = 2, col = "gray")



# ----------
tdat <- diff(airquality$Wind)


plot(tdat, type = "l")

lines(smooth.spline(time(tdat), tdat, spar = 0.5), lwd = 2, col = "blue")

lines(smooth.spline(time(tdat), tdat, spar = 0.75), lwd = 2, col = "red")

abline(v = c(30 * 1:5), lty = 2, col = "gray")




# ------------------------------------------------------------------------------
# data exploration:  smoothing for Temp
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


tdat <- airquality$Temp


plot(tdat, type = "l")

lines(smooth.spline(time(tdat), tdat, spar = 0.5), lwd = 2, col = "blue")

lines(smooth.spline(time(tdat), tdat, spar = 0.75), lwd = 2, col = "red")

abline(v = c(30 * 1:5), lty = 2, col = "gray")



# ----------
tdat <- diff(airquality$Temp)


plot(tdat, type = "l")

lines(smooth.spline(time(tdat), tdat, spar = 0.5), lwd = 2, col = "blue")

lines(smooth.spline(time(tdat), tdat, spar = 0.75), lwd = 2, col = "red")

abline(v = c(30 * 1:5), lty = 2, col = "gray")



# -->
# somewhat monthly cycle ..


