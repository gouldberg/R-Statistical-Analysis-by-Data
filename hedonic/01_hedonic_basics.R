setwd("//media//kswada//MyFiles//R//hedonic")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hedonic
#  - Harrison and Rubinfeld (1978) consider the median values of owner-occupied homes ("mv") in a cross section of 506 census tracts from 92 towns in the Boston area.
#    Values are explained by a combination of tract- and town-level variables.:
#       - Observed at tract level:  Crime rate (crim), pollution (nox), average number of rooms (rm) and age, distance to employment centers (dis),
#         and proportion of blacks in the population
#       - Observed at town level:  proportion of industrial dwellings (indus), distance to radian highways (rad), property tax rate,
#         and pupil-to-teacher ratio on local schools (ptratio)
#       - The town identifier for each tract (townid) allows to account for clustering within each town, which may comprise from 1 to 30 tracts. 
# ------------------------------------------------------------------------------

data("Hedonic", package = "plm")


str(Hedonic)


dim(Hedonic)


car::some(Hedonic)



# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


