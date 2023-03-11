setwd("//media//kswada//MyFiles//R//infant_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  infant growth
# ------------------------------------------------------------------------------

data("infantGrowth", package = "fda")


dim(infantGrowth)


head(infantGrowth)



# ------------------------------------------------------------------------------
# data exploration:  plot trajectory
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(tibiaLength ~ day, data = infantGrowth, type = "o")



# -->
# The staircase nature of growth in this early period and need to estimate the velocity of change in bone length,
# makes monotone smoothing essential.

# It seems astonishing that this small bone in the baby's lower leg has the capacity to grow as much as two millimeters in a single day.
