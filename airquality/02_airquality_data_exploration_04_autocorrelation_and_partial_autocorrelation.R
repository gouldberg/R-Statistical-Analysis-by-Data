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
# assess stationarity orders
# ------------------------------------------------------------------------------

forecast::ndiffs(airquality$Wind)


forecast::ndiffs(airquality$Temp)




# ------------------------------------------------------------------------------
# Correlation analysis:  sample autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------


astsa::acf2(diff(airquality$Wind))


astsa::acf2(diff(airquality$Temp))







