setwd("//media//kswada//MyFiles//R//air_crash")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  AirCrash
# ------------------------------------------------------------------------------
data("AirCrash", package = "vcdExtra")

data <- AirCrash

data


tab <- xtabs(~ Phase + Cause, data = data)

tab



# ------------------------------------------------------------------------------
# display the pattern of signs and magnitudes of the residuals
# ------------------------------------------------------------------------------
# use 5-characters abbreviation for both levels and rotate left labels by -45 degrees
mosaic(tab, expected = ~ Phase + Cause, shade = TRUE, 
       labeling_args = list(abbreviate_labs = c(Cause = 5, Phase = 5), rot_labels = c(bottom = 90, left = -45)), interpolate=1:4)



# reordering
phase_order <- c("landing", "en route", "take-off", "standing", "unknown")
cause_order <- c("human error", "mechanical", "weather", "unknown", "criminal")

mosaic(tab[phase_order, cause_order], expected = ~ Phase + Cause, shade = TRUE, 
       labeling_args = list(abbreviate_labs = c(Cause = 5, Phase = 5), rot_labels = c(bottom = 90, left = -45)), interpolate = 1:4)





