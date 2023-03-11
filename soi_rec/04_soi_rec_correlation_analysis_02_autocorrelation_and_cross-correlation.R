setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and cross-correlation (by "ccf" function)
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3, 1))

acf(soi, lag = 48, main = "Southern Oscillation Index")

acf(rec, lag = 48, main = "Recruitment")

ccf(soi, rec, lag = 48, main = "SOI vs Recruitment", ylab = "CCF")




# -->
# Both of the ACFs exhibit periodicities corresponding to the correlation between values separated by 12 units.
# Observations 12 months or one year apart are strongly positively correlated, as are observations at multiples such as 24, 36, 48, ...
# Observations separated by six months are negatively correlated, showing that positive excursions tend to be associated with negative excursions six months
# removed.


# -->
# Sample CCF, however, shows some departure from the cyclic component of each series and there is an obvious peak at h = -6.
# This result implies that SOI measured at time t - 6 months is associated with the Recruitment series at time t.
# We could say the SOI leads the Recruitment series by six months.

# The sign of the CCF is negative, leading to the conclusion that
# the two series more in different directions; that is,
# increases in SOI lead to decreases in Recruitment and vice versa.



# The dashed lines shown on the plots indicate +- 2 / sqrt(453) = +- 2 / sqrt(n);
# large sample distribution of cross-correlation is normal with mean zero and standard deviation = 1 / sqrt(n)
# if at least one of the processes is independent white noise.

# But since neither series is noise, these lines do not apply.

# in order for the dashed lines to be significant, at least one of the series must be white noise
# If this is not the case, there is no simple way to tell if a cross-correlation estimate is significantly different from zero
# We are only guessing at the linear dependence relationship between SOI and Recruitment



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and cross-correlation (by "acf" function)
# ------------------------------------------------------------------------------

par(mar = c(4,4,4,4))

acf(cbind(soi, rec))

