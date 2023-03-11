setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part




# ------------------------------------------------------------------------------
# Correlation analysis:  lagged scatterplot matrices
#   - to check for nonlinear relations
# ------------------------------------------------------------------------------

graphics.off()

astsa::lag2.plot(part, cmort, max.lag = 20)


forecast::Ccf(part, cmort, lag.max = 20, plot=FALSE)



# -->
# shows a fairly strong linear relationship between Mortality and Pollution with lag 4,
# indicating the Pollution series tends to lead the Mortality series around 4 weeks





# ----------
graphics.off()

astsa::lag2.plot(tempr, cmort, max.lag = 20)


forecast::Ccf(tempr, cmort, lag.max = 20, plot=FALSE)




# ------------------------------------------------------------------------------
# Check by pair plot
# ------------------------------------------------------------------------------


psych::pairs.panels(cbind(cmort, stats::lag(part, -4), stats::lag(tempr, -1)))




