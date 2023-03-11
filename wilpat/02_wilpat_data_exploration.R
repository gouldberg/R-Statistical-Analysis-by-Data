setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)

dim(WilPat)


head(WilPat)



# ------------------------------------------------------------------------------
# distribution of each variables
# ------------------------------------------------------------------------------

summary(WilPat)


Hmisc::describe(WilPat)



