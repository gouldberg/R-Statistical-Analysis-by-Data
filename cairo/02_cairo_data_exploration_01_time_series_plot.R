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
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(2,1))

plot(temp ~ time, data = cairo, type = "l", ylab = "temperature (F)", xlab = "time (days)")


# growth rate
plot(diff(cairo$temp), type = "l")




# ----------
forecast::ndiffs(cairo$temp)



# -->
# It is clear that the data contain a good deal of noisy short term auto-correlation,
# and a strong yearly cycle.
# Much less clear, given these other sources of variation, is whether there is evidence for any increase in average mean temperature over the period.
# This is the sort of uncertainty that allows climate change sceptics of bamboozle the ill-informed.
