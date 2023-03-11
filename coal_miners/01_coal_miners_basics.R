setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
#   - Data from Ashford & Sowden (1970) given by Agresti (1990) on the association between two pulmonary conditions, 
#     breathlessness and wheeze, in a large sample of coal miners who were smokers
#     with no radiological evidence of pneumoconlosis, aged between 20???64 when examined.
#     This data is frequently used as an example of fitting models for bivariate, binary responses.
#   - Agresti (2002) cites data from Ashfold and Sowden (1970) on the association between two pulmonary conditions,
#     breathlessness and wheeze, in a large sample of coal miners.
#     The miners are classified into age groups, and the question treated by Agresti is 
#     whether the association between these two symptom is homogeneous over age.
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age

dim(data)


dimnames(data)


data



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

