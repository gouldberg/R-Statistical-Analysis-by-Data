setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
#   - dataset from Koller and Alexandrowicz (2010), using the Neuropsychological Test Battery for Number Processing and Calculation in Children (ZAREKI-R)
#     for the assessment of dyscalculia in children.
#   - There are n = 341 children (2nd to 4th year of elementary school) in their sample, eight items on addition,
#     eight items on subtraction, and two covariates (time needed for completion in minutes as well as grade)
#   - calss: elementary school class
#   - time:  time in min require to complete the test
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
