setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------
data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

