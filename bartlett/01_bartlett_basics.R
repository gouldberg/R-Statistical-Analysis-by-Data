setwd("//media//kswada//MyFiles//R//bartlett")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bartlett data on plum-root cuttings
#  - the result of an agricultural experiment to investigate the survival of plum-root cuttings (Alive) in relation to two factors
#      - Time of planting  and  the Length of the cutting
#  - In this experiment, 240 cuttings were planted for each of the 2 * 2 combinations of these factors, and their survival 8Alive, Dead) was later recorded.
#  - This table is used by Bartlett 81935) to illustrate a method for testing for no 3-way interaction in a contingency table
# ------------------------------------------------------------------------------
data("Bartlett", package = "vcdExtra")

data <- Bartlett

data



# ------------------------------------------------------------------------------
# Mosaic matrices
#   - The vcd packages extends pairs() generic function to mosaic matrices with methods for "table" and "structable" objects
# ------------------------------------------------------------------------------
# The mosaic matrix show all 2-way marginal relations
# Time and Length are independent are independent by the design of the experiment  (gp = shading_Friendly here to emphasize this)
pairs(data, gp = shading_Friendly2)



# --> Greater survival is associated with cuttings taken now (vs. spring) and those cut long (vs. short),
# and the degree of association is stronger for planting time than for cutting length.





