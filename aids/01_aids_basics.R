setwd("//media//kswada//MyFiles//R//aids")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aids
#  - Consist of the quarterly reported AIDS cases in the U.K. from January 1983 to March 1994 obtained from the Public Health Laboratory Service,
#    Communicable Disease Surveillance Centre, London.
#  - Variable:
#      - y:  the number of quarterly aids cases in England and Wales
#      - x:  time in quarters from January 1983
#      - qtr:  a factor for the quarterly seasonal effect (1, 2, 3, 4)
# ------------------------------------------------------------------------------
data("aids", package = "gamlss.data")


str(aids)

car::some(aids)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

car::scatterplot(y ~ x, data = fabric)



