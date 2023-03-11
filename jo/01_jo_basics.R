setwd("//media//kswada//MyFiles//R//jo")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  JO
#   - The data table confronts, in the rows, the different athletic events and, in thecolumns, the different countries.
#     Each cell contains the total number of medals (gold, silver, and bronze) won at the Olympiads between 1992 and 2008
#     (Barcelona 1992, Atlanta 1996, Sydney 2000, Athens 2004, Beijing 2008)
# ------------------------------------------------------------------------------

data("JO", package = "FactoMineR")

str(JO)

dim(JO)


head(JO)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
