setwd("//media//kswada//MyFiles//R//tulips")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tulips
#   - sizes of blooms from beds of tulips grown in greenhouses, under different soil and light conditions.
#   - The "water" indicates one of three ordered levels of light exposure from high to low
#   - The last column "bed" indicates a cluster of plants from the same section of the greenhouse.
# ------------------------------------------------------------------------------
data("tulips", package = "rethinking")

d <- tulips

dim(d)

str(d)


