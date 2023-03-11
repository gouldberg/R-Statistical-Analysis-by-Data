setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")


str(TobinQ)

dim(TobinQ)



# ----------
pTobinQ <- pdata.frame(TobinQ)



# ------------------------------------------------------------------------------
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------

# Variance decomposition:  "qn" is covariate in this example
summary(pTobinQ$qn)



# -->
# The variation is mainly due to inter-individual, but less than half (43.1%)





