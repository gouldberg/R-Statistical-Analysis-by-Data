setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\film_process")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Film Extrusion Process
#   - The data arose from a planned experiment in which the Heater current and Valve position were adjusted
#     aproximately every 5 seconds, according to a table of random normal values.
#     In practice these input settings had to be occasionally modified to keep the process stable and the adjustments
#     were made slightly more slowly than the sampling interval of 5 seconds,
#     so there is some slight autocorrelation in their values.
#     The data were supplied by ICI Ltd. in 1968 for a PhD project and were scaled to give some commercial protection
#     as their analysis was publically available
#   - Variables:  heater current, the valve position, pressure, width
#   - We estimate the response of the times series of Extrusion pressure to the Heater current and Valve setting
# ------------------------------------------------------------------------------


film <- read.table("FilmProcess.txt", sep = "", header = F, colClasses = "numeric")


head(film)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

MTS::MTSplot(film)





# ------------------------------------------------------------------------------
# data exploration:  Smoothing
# Smoothing splines
# ------------------------------------------------------------------------------


hc <- film$V1

vp <- film$V2

prs <- film$V3


# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

graphics.off()

par(mfrow=c(3,1))


plot(hc, type = "l")
abline(v = seq(0, 350, by = 25), lty = 2, col = "gray")
lines(smooth.spline(time(hc), hc, spar = 0.5), lwd = 2, col = "blue")
lines(smooth.spline(time(hc), hc, spar = 1), lwd = 2, col = "red")



plot(vp, type = "l")
abline(v = seq(0, 350, by = 25), lty = 2, col = "gray")
lines(smooth.spline(time(vp), vp, spar = 0.5), lwd = 2, col = "blue")
lines(smooth.spline(time(vp), vp, spar = 1), lwd = 2, col = "red")



plot(prs, type = "l")
abline(v = seq(0, 350, by = 25), lty = 2, col = "gray")
lines(smooth.spline(time(prs), prs, spar = 0.5), lwd = 2, col = "blue")
lines(smooth.spline(time(prs), prs, spar = 1), lwd = 2, col = "red")





# ------------------------------------------------------------------------------
# forecast::ndiffs and differenced series
# ------------------------------------------------------------------------------


# output (pressure and width) are non-stationary
apply(film, 2, forecast::ndiffs)




graphics.off()

par(mfrow=c(1,1))


plot(diff(prs), type = "l")
lines(smooth.spline(time(diff(prs)), diff(prs), spar = 0.5), lwd = 2, col = "blue")
lines(smooth.spline(time(diff(prs)), diff(prs), spar = 1), lwd = 2, col = "red")


mean(diff(prs))


