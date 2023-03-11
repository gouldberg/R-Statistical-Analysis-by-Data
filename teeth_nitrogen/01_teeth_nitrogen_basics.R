# setwd("//media//kswada//MyFiles//R//teeth_nitrogen")
setwd("//media//kswada//MyFiles//R//Variance_and_correlation_structure_model//teeth_nitrogen")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TeethNitrogen
#   - Data on nitrogen isotope ratios in teeth of stranded whales. One of which became famous and attracted newspaper headlines
#     when it stranded in Edinburgh, Scotland, and was nicknamed "Moby the whale"
# ------------------------------------------------------------------------------


TN <- read.table(file = "TeethNitrogen.txt", header = TRUE)


str(TN)


dim(TN)


car::some(TN)



# ------------------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------------------

