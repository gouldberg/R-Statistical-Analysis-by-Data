setwd("//media//kswada//MyFiles//R//space_shuttle")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Space Shuttle
#   - The space shuttle Challenger exploded 73 seconds after take-off on January 28, 1986.
#     Subsequent investigation presented to the presidential commission headed by William Rogers determined that the cause was failure of the O-ring seals
#     used to isolate the fuel supply from burning gases.
#   - Engineers from Morton Thiokol, manufacturers of the rocket motors, had been worried about the effects of unseasonably cold weather on the O-ring seals
#     and recommended aborting the flight. NASA staff analyzed the data, tables, and charts submitted by the engineers and concluded that there was
#     insufficient evidence to cancel the flight.
#     However, the engineers omitted the observations where no O-rings failed or showed signs of damage, believing that they were informative.
#     Unfortunately, those observations had occurred when the launch temperature was relatively warm and were indeed informative.
#   - The data set in vcd contains data on the failures of the O-rings in 24 NASA launches preceding the launch of Challenger, as given by Dalal et al. (1989)
#     and Tufte (1997), also analyzed by Lavine (1991).
#   - Each launch used two booster rockets with a total of six O-rings, and the data set records as nFailures the number of these that were considered damaged
#     after the rockets were recovered at sea. In one launch (gflight #4), the rocket was lost at sea, so the relevant response variables are missing.
# ------------------------------------------------------------------------------
data("SpaceShuttle", package = "vcd")

data <- SpaceShuttle

data



# ------------------------------------------------------------------------------
# Scatter plot by temperature(F) and number of incidents
# ------------------------------------------------------------------------------
plot(data$Temperature, data$nFailures, 
     xlab = "Temperature(F)", ylab = "Number of incidents", 
     xlim = c(50, 85), ylim = c(0, 3))

text(data$Temperature, data$nFailures+0.2,labels=data$FlightNumber)

