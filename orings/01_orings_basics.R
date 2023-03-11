setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
#   - In January 1986, the space shuttle Challenger exploded shortly after launch.
#     An investigation was launched into the cause of the crash and attention focused on the rubber O-ring seals in the rocket boosters.
#     At lower temperatures, rubber becomes more brittle and is a less effective sealant.
#     At the time of the launch, the temperature was 31F.
#   - In the 23 previous shuttle missions for which data exists, some evidence of damage due to blow by and erosion was recorded on some O-rings.
#     Each shuttle had two boosters, each with three O-rings. For each mission, we know the number of O-rings out of six showing some
#     damage and the launch temperature.
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------



