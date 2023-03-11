setwd("//media//kswada//MyFiles//R//rmotivation")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rmotivation
#   - Dataset for study of motivational structures (intrinsic, extrinsic, hybrid) of R package authors presented in Mair et al. (2015).
#     The authors were interested in exploring the question:  What motivates package developers to contibute to the R environment ?
# ------------------------------------------------------------------------------

data("Rmotivation", package = "MPsychoR")


str(Rmotivation)



# ----------
# Here we focus on hybrid motivation only and selct 19 dichotomous items associated with this latent variable

ind <- grep("hyp", colnames(Rmotivation))

HybMotivation <- na.omit(Rmotivation[, ind])

k <- ncol(HybMotivation)





