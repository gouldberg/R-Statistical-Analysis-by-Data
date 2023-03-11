setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# fit ARIMA model for Wind
# ------------------------------------------------------------------------------

library(astsa)

sarima(airquality$Wind, p = 2, d = 1, q = 1, no.constant = TRUE)



# ----------
# ar2 is NOT significant  --> p = 1

sarima(airquality$Wind, p = 1, d = 1, q = 1, no.constant = TRUE)



# -->
# but p-values for Ljung-Box statistic shows some auto-correlation



# ------------------------------------------------------------------------------
# fit ARIMA model for Temp
# ------------------------------------------------------------------------------

library(astsa)

sarima(airquality$Temp, p = 2, d = 1, q = 1, no.constant = TRUE)



# -->
# residulas are good, but the ar and ma coefs are NOT significant

