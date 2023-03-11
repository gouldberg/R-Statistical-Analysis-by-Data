setwd("//media//kswada//MyFiles//R//cairo")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cairo
#   - daily temperature in Cairo over nearly a decade.
#     The data are from:   http:://www.engr.udayton.edu/weather/citylistWorld.htm
# ------------------------------------------------------------------------------

data("cairo", package = "gamair")

str(cairo)

head(cairo)



# ----------
car::some(cairo)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
