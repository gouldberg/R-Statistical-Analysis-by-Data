setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# Data transformation
# ------------------------------------------------------------------------------

# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)


MTSplot(x)




# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Cd, Ud)




# ------------------------------------------------------------------------------
# Plot growth rate
# ------------------------------------------------------------------------------

# growth rate
gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))

plot(100 * gr, main = "Growth Rate (%)")




# ------------------------------------------------------------------------------
# Scale to variance 1
# ------------------------------------------------------------------------------


# scale each series to have variance 1
# scaling strips ts attributes

gr <- ts(apply(gr, 2, scale), freq = 4)




# ----------
# check cross-correlation

acf(gr)





# ------------------------------------------------------------------------------
# Spectral estimation
# ------------------------------------------------------------------------------

# degrees of smoothing
# L <- c(7, 7)
L <- c(9, 9)



# Spectral estimation for detrend, standardized and tapered growth rate

gr.spec <- mvspec(gr, spans = L, demean = FALSE, detrend = FALSE, taper = 0.25)




# ----------
graphics.off()

plot(gr.spec, log = "no", main = "Individual Spectra", lty = 1:5, lwd = 2)
abline(v = c(0.125, 0.25, 1.0), lty = 1, col = "blue")

legend("topright", colnames(econ5), lty = 1:5, lwd = 2)



# -->
# "1" means:  1 cycle every 4 quarters


# -->
# We focus on 3 interesting frequencies.
# First, we note the lack of spectral power near the annual cycle (omega = 1, or one cycle every four quarters), 
# indicating the data have been seasonally adjusted.
# In addition, because of the seasonal adjustment, some spectral power appears near the seasonal frequency;
# This is a distortion apparently caused by the method of seasonally adjusting the data.

# Next, we note the spectral power near omega = 0.25, or one cycle every 4 years, in umployment, GNP, consumption, and, to lesser degree,
# in private investment.

# Finally, spectral power appears near omega = 0.125, or one cycle every 8 years in government investment,
# and perhaps to lesser degrees in unemployment, GNP, and consumption.

