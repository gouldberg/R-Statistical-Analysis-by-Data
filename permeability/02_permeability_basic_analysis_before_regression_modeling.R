setwd("//media//kswada//MyFiles//R//permeability")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  permeability
# ------------------------------------------------------------------------------
data("permeability", package = "AppliedPredictiveModeling")

dim(fingerprints)
dim(permeability)


car::some(fingerprints)


# ----------
# sparse data
fingerprints[1:10, 1:20]


permeability



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  in this case, nearZeroVar
# ------------------------------------------------------------------------------
# 719 out of 1,107 predictors is near zero variance variable ...
length(nearZeroVar(fingerprints))



# ----------
# This produces errors ...
length(findCorrelation(cor(fingerprints), cutoff = 0.75))


