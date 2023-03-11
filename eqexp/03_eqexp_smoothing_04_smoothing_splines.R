setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)


# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name




# ------------------------------------------------------------------------------
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness


par(mfrow=c(1,1))


# ----------
# Earth Quakes

obj_ts <- EQ5

plot(obj_ts, type = "l")

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.25), lwd = 2, col = 4)

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.5), lty = 2, lwd = 2, col = 2)




# ----------
# EX5

obj_ts <- EX5

plot(obj_ts, type = "l")

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.25), lwd = 2, col = 4)

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.5), lty = 2, lwd = 2, col = 2)



# ----------
# NZ

obj_ts <- NZ

plot(obj_ts, type = "l")

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.25), lwd = 2, col = 4)

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.5), lty = 2, lwd = 2, col = 2)

