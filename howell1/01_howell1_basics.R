setwd("//media//kswada//MyFiles//R//howell1")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Howell1
#   - partial census data for the Dobe area !King San, compiled from interviews conducted by Nancy Howell in the late 1960s.
#   - For the non-anthropologists reading along, the !King San are the most famous foraging population of the 20th centry, largely because of detailed
#     quantitative studies by people like Howell.
#   - 544 individuals has a recorded height (centimeters), weight (kilograms), age (years), and "maleness" (0 indicating female and 1 indicating male).
# ------------------------------------------------------------------------------
data("Howell1", package = "rethinking")

d <- Howell1

dim(d)

str(d)

