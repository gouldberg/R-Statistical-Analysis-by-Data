setwd("//media//kswada//MyFiles//R//rogers")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rogers
#   - dataset on comorbid obsessive compulsive disorder (OCD) and depression contains 16 depression items (QIDS; 5-point scale)
#     and 10 OCD items (Y-BOCS;4-point scale) for 408 patients in the sample
# ------------------------------------------------------------------------------

data("Rogers", package = "MPsychoR")

str(Rogers)

dim(Rogers)

car::some(Rogers)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------



