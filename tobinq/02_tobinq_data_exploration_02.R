setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")


str(TobinQ)

dim(TobinQ)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


