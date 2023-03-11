setwd("//media//kswada//MyFiles//R//speech")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  speech
# ------------------------------------------------------------------------------

data(speech, package = "astsa")

str(speech)


head(speech)



# ------------------------------------------------------------------------------
# Fit SARIMA model
# ------------------------------------------------------------------------------


astsa::sarima(speech, p = 4, d = 0, q = 4)


astsa::sarima(speech, p = 4, d = 0, q = 0)


astsa::sarima(speech, p = 12, d = 0, q = 0)



# -->
# Difficult to find appropriate orders
