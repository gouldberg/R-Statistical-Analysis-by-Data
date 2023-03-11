setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SSOCS
# ------------------------------------------------------------------------------

load(file="Chapter4_ssocs08.Rdata")

dim(SSOCS.data)

str(SSOCS.data)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

