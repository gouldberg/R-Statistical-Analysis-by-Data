setwd("//media//kswada//MyFiles//R//air_passengers")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Air Passengers
#   - Monthly totals of international airline passengers, 1949 to 1960, taken from Box and Jenkins.
# ------------------------------------------------------------------------------

data(AirPassengers, package = "datasets")

str(AirPassengers)

head(AirPassengers)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow=c(1,1))

plot(AirPassengers, type = "o", ylab = "")


# growth rate
plot(diff(log(AirPassengers)), type = "l")




# ----------
mean(diff(log(AirPassengers)))



# -->
# average growth rate is 0.94%


# ----------
forecast::ndiffs(log(AirPassengers))



# ------------------------------------------------------------------------------
# data exploration:  time series plot by ts.plot and monthplot
# ------------------------------------------------------------------------------

x <- AirPassengers

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 12)



# ----------
plot.ts(cbind(x, lx, dlx, ddlx), main = "")



# -->
# shows trend plus increasing variance.
# log transformation stabilizes the variance
# logged data and then differenced to remove trend
# It is clear the there is still persistence in the seasons, so that a twelfth-order difference
# The transformed data appears to be stationary





# -----------
par(mfrow = c(2,1))

monthplot(dlx)

monthplot(ddlx)



# -->
# The level of dlx is much different by month


