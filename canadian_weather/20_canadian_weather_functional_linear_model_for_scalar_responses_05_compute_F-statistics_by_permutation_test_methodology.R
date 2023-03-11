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
# Compute F-statistic by permutation test methodology
# ------------------------------------------------------------------------------

# Because of the nature of functional statistics, it is difficult to attempt to derive a theoretical null distribution for any given test statistic
# since we would need to account for selecting a smoothing parameter as well as the smothing itself.
# Instead, the package employs a permutation test methodology.
# This involves constructing a null distribution from the observed data directly.

# The advantage of this is that we no longer need to rely on distributional assumptions.
# The disadvantage is that we cannot test for the significance of an individual covariate among many.


F.res = Fperm.fd(annualprec, templist, betalist)


# obseerved F statistic
F.res$Fobs


# 95the quantile of permutation distribution
F.res$qval


# -->
# The observed F statistic is 3.03, while the 95th quartile of the permutation distribution is 0.27 ??,
# giving strong evidence for the effect.
