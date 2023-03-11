setwd("//media//kswada//MyFiles//R//mental")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Mental
#   - Srole et al. (1978) gave the data on the mental health status of a sample of 1660 young New York residents
#     in midtown Manhattan classified by their parents' socioeconomic status (SES)
#   - These data have also been analyzed by many authosrs, including Agresti, Goodman, and Haberman.
#   - There are six categories of SES (from 1 = "High" to 6 = "Low"), and mental health is classified in the four categories
#     "well", "mild symptom formation", "moderate symptom formation", and "impaired".
# ------------------------------------------------------------------------------
data("Mental", package = "vcdExtra")

data <- Mental

data



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
