setwd("//media//kswada//MyFiles//R//ocp")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  OCP
#   - Bilsky and Jehn (2002) and Borg et al. (2011) use data based on the organizational culture profile (OCP).
#     The OPC is an instrument that contains a set of value statements that can be used to idiographically assess both the extent
#     to which certain values characterize a target organization and an individual's preference for that particular configuration of values.
#   - The OCP requires individuals to sort 54 items into nine ordered categories.
#     Most of the 54 items can be classified into four classes derived from Schwarz' theory of values (Schwartz, 1992); conservation, openness to change,
#     self-transcendence, and self-enhancement.
#     Twelve items remain unclassified.
# ------------------------------------------------------------------------------

data("OCP", package = "smacof")

str(OCP)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

psych::describe(OCP)

