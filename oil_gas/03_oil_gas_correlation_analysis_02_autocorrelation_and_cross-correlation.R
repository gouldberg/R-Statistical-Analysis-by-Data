setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")
data(gas, package = "astsa")

str(oil)
str(gas)

oil
gas



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and cross-correlation
# ------------------------------------------------------------------------------

# Not-transformed data
graphics.off()
par(mfrow = c(3, 1))

Acf(gas, lag = 48, main = "Gas")

Acf(oil, lag = 48, main = "Oil")

Ccf(gas, oil, lag = 48, main = "Gas vs Oil")



# ----------
# Transformed data

graphics.off()

par(mfrow = c(3, 1))

Acf(diff(log(gas)), lag = 48, main = "Gas: difference of logged")

Acf(diff(log(oil)), lag = 48, main = "Oil: difference of logged")

Ccf(diff(log(oil)), diff(log(gas)), lag = 48, main = "Oil vs. Gas")




# -->
# The small, but significant values when gas leads oil might be considered as feedback
# Oil prices is leading 3 weeks
# BUT NOTE that Gas prices is leading 1 weeks too ...  (due to expection of future price rising)


# The dashed lines shown on the plots indicate +- 2 / sqrt(545) = +- 2 / sqrt(n);
# large sample distribution of cross-correlation is normal with mean zero and standard deviation = 1 / sqrt(n) if at least one of the processes is
# independent white noise.

# But since neigher series is noise, these lines do not apply.

# in order for the dashed lines to be significant, at least one of the series must be white noise
# If this is not the case, there is no simple way to tell if a cross-correlation estimate is significantly different from zero
# We are only guessing at the linear dependence relationship between Oil and Gas time series ...

