setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(jj, type = "o", ylab = "Quarterly Earnings per Share")



# -->
# Note the gradually increasing underlying trend and the rather regular variation superimposed on the trend that
# seems to repeat over quarters.



# ----------
# growth rate
plot(diff(log(jj)), type = "l")



# ----------
mean(diff(log(jj)))


# -->
# average growth rate is 3.3%   (large growth rate)


# ----------
forecast::ndiffs(log(jj))



# ------------------------------------------------------------------------------
# data exploration:  time series plot by ts.plot and monthplot
# ------------------------------------------------------------------------------

x <- jj

lx <- log(x)

dlx <- diff(lx)


# take difference of same quarter of last year
ddlx <- diff(dlx, 4)



# ----------
plot.ts(cbind(x, lx, dlx, ddlx), main = "")



# -->
# shows trend plus increasing variance.
# log transformation stabilizes the variance
# logged data and then differenced to remove trend
# It is clear the there is still persistence in the quarterly seasons, so that a 4th order difference
# The transformed data appears to be stationary



# -----------
par(mfrow = c(2,1))

monthplot(dlx)

monthplot(ddlx)

