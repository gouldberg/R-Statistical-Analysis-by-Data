setwd("//media//kswada//MyFiles//R//speech")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  speech
#   - small .1 second (1000 point) sample of recorded speech for the phrase aaa ...hhh  (sampled at 10,000 points per second)
# ------------------------------------------------------------------------------

data(speech, package = "astsa")

str(speech)


head(speech)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

