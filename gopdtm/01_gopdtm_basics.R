setwd("//media//kswada//MyFiles//R//gopdtm")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GOPdtm
#   - Mair et al. (2014) report application of the gravity model.
#     They studied the semantic space of certain self-reported statements of Republican voters in the USA.
#     These voters were asked to complete the sentence "I'am Republican, because ...". 
#     Their responses are published on the official Web site of the Republican party.
#     252 unique statements and the 35 most frequent key words were identified by text analysis methods.
#     They were used to form a 252 * 35 incidence matrix Z with cells of 1 if statement i contains word j, and zero if not.
# ------------------------------------------------------------------------------

data(GOPdtm, package = "smacof")


str(GOPdtm)


car::some(GOPdtm)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


