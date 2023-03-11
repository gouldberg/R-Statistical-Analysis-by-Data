setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
#   - 234 height measurements on 26 boys from an Oxford Boys Club, at 9 different ages (centered and standardized) per boy.
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
