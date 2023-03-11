# setwd("//media//kswada//MyFiles//R//boreality")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//boreality")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  boreality
#   - Using remotely sensed data and spatial statistical methods, Zuur et al. explored the influence of relief, soil,
#     and climatic factors on the forests of the Raifa section of Volzhsko-Kamsky State Nature Biosphere.
#   - The response variable is a boreal forest index and is defined as the number of species that belong to a set of boreal species
#     divided by the total number of species ata site.
#   - Several remotely sensed variables derived from the LANDSAT5 sattellite images were used as explanatory variable:
#        - the normalised difference vegetation index
#        - temperature
#        - an index of wetness
#        - an index of greenness
# ------------------------------------------------------------------------------


Boreality <- read.table(file = "Boreality.txt", header = TRUE)


str(Boreality)


dim(Boreality)


car::some(Boreality)



# ------------------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------------------

