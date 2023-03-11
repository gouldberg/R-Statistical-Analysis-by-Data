setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  canadian weather
# ------------------------------------------------------------------------------

data("CanadianWeather", package = "fda")

str(CanadianWeather)



# ----------
CanadianWeather$dailyAv[,,'Temperature.C']

CanadianWeather$dailyAv[,,'Precipitation.mm']

CanadianWeather$dailyAv[,,'log10precip']

CanadianWeather$place

CanadianWeather$province

CanadianWeather$coordinates

CanadianWeather$monthlyTemp

CanadianWeather$monthlyPrecip




# ------------------------------------------------------------------------------
# Test for no-effect of geographic region on temperature profile
# Functional F-tests by fosr()
# ------------------------------------------------------------------------------

# fosr.perm():  permutation testing for function-on-scalar regression
# prelim:  number of preliminary permutations, the smoothing parameter in the main permutations will be fixed to the median value from these preliminary permutations.
regionperm = fosr.perm(fdobj = tempfd, X = modmat, con = constraints, method = "OLS", nperm = 200, prelim = 30)



# Pointwise F-type statistics at each of the points given by argvals
regionperm$F

regionperm$argvals


# a matrix, each of whose rows gives the pointwise F-type statistics for a permuted data set
regionperm$F.perm



# ----------
par(mfrow=c(1,1))
plot(regionperm, axes = FALSE, xlab = "")
box()
axis(2)
axisIntervals(1)



# -->
# The continuous blue curve is the function
# The dashed red line is the critical level (0.95),
# The gray lines are the N! functions (N! permutations)

# If the maximum of the continuous blue curve is above the dashed critical level, we reject H0
