setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
#   - The Motion Analysis Laboratory at Children's Hospital, San Diego, CA, collected data, which consist of the angles fomred by the hip and knee
#     of each of 39 children over each child's gait cycle. See Olshen et al. (1989) for full details.
#   - Time is measured in terms of the individual gait cycle, which we have translated into values of t in [0,1].
#     The cycle begins and ends at the point where the heel of the limb under observation strikes the ground.
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

