setwd("//media//kswada//MyFiles//R//helm")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  helm
#   - Data for an experiment on color perception.
#     Helm (1964) asked a sample of test persons to assess the similarity of ten chips with different colors (same brightness and same saturation).
#     For each individual, similarity scores were obtained for each pair of colors.
#     Some test persons were deuteranopic to some extent; i.e., they were not able to clearly distinguish green and purplish-red ("red-green blind")
# ------------------------------------------------------------------------------

data(helm, package = "smacof")


str(helm)


attributes(helm)


car::some(helm)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


