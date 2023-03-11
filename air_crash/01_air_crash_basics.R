setwd("//media//kswada//MyFiles//R//air_crash")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  AirCrash
#  - databse of all crashes of commercial airplanes between 1993 - 2015, classified by Phase of the flight and Cause of the crash.
# ------------------------------------------------------------------------------
data("AirCrash", package = "vcdExtra")

data <- AirCrash

data


tab <- xtabs(~ Phase + Cause, data = data)


tab


