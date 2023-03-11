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
# ------------------------------------------------------------------------------

# Permutation test for a predictive relationship between geographic region and temperature profile for the Canadian weather data.
F.res = Fperm.fd(temp36fd, regionList, betaList)


# The observed p-value of the permutation test
F.res$pval



# ----------

par(mfrow=c(1,1))

with(F.res,{
  q = 0.95
  ylims = c(min(c(Fvals, qval, qvals.pts)), max(c(Fobs,qval)))
  plot(argvals, Fvals, type = "l", ylim = ylims, col = 1,
       lwd = 2, xlab = "day", ylab = "F-statistic", cex.lab=1.5,cex.axis=1.5)
  lines(argvals, qvals.pts, lty = 3, col = 1, lwd = 2)
  abline(h = qval, lty = 2, col = 1, lwd = 2)
  legendstr = c("Observed Statistic", paste("pointwise", 1 - q, "critical value"), paste("maximum", 1 - q, "critical value"))
  legend(argvals[1], 1.2, legend = legendstr, col = c(1, 1, 1), lty = c(1, 3, 2), lwd = c(2, 2, 2))
}
)


