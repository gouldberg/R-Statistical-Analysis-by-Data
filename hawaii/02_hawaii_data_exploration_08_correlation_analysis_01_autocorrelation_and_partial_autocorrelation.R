setwd("//media//kswada//MyFiles//R//chicken")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chicken
# ------------------------------------------------------------------------------

data(chicken, package = "astsa")

str(chicken)

head(chicken)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3, 1))


par(mfrow = c(2,1))


astsa::acf2(sqrt(Hawaii2$Moorhen.Kauai), max.lag = 20, main = "Moorhen.Kauai")

astsa::acf2(resid(fit), max.lag =20, main = "detrended")

astsa::acf2(diff(sqrt(Hawaii2$Moorhen.Kauai)), max.lag = 20, main = "first difference")



# -->
# 1st diffence is somewhat stationary



