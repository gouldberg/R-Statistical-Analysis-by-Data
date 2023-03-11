setwd("//media//kswada//MyFiles//R//powerplant")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "timsac")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Power Plant
#   - 500 observations on 3 variables:  comamnd, temperature and fuel
# ------------------------------------------------------------------------------

data(Powerplant, package = "timsac")


dim(Powerplant)


str(Powerplant)


head(Powerplant)


colnames(Powerplant) <- c("command", "temperature", "fuel")




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and cross-correlation
# ------------------------------------------------------------------------------

graphics.off()


acf(Powerplant[,c(2,3,1)])


acf(diff(Powerplant[,c(2,3,1)]))



# ----------
astsa::lag2.plot(Powerplant[,"command"], Powerplant[,"fuel"], max.lag = 24)
