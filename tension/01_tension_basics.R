setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
#   - Data taken from Vines et al. (2006), also analyzed in Levitin et al. (2007).
#     The authors were interested in how physical gestures of professional musicians contribute to the perception of emotion in a musical performance.
#   - 29 participants were exposed to the performance by either just listening (auditory condition), just seeing (visual condition), or both
#     (auditory-visual condition).
#     During the performance the participants had to move a slider to indicate the experienced tension they felt.
#     They listened to the piece of 80s, and every 10ms the tension score (0-127) was recorded which leaves us with 800 tension measurement points per person.
#   - Note that the original tension scores (0-127) were z-standardized.
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)


dim(tension)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
