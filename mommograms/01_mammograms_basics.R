setwd("//media//kswada//MyFiles//R//mammograms")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Mammogram ratings
#  - 4 * 4 table of (probably contrived) ratings os 110 mammograms by two raters.
#    The data from Kundel and Polansky (2003), used to illustrate the calculation and interpretation of agreement measures in this context.
# ------------------------------------------------------------------------------
data("Mammograms", package = "vcdExtra")

data <- Mammograms

data


# ----------
sieve(data, shade=TRUE)
