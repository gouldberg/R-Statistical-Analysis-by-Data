setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
#   - dataset of modified version of the Wilson-Patterson conservatism scale (Wilson and Patterson, 1968).
#     We use the first 15 "conservative" items. Each item has three response categories: 1 = "approve", 0 = "disapprove", and 2 = "don't know".
#     Note that the response categories is not ordinal but nominal.
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")


str(WilPat)


dim(WilPat)


head(WilPat)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
