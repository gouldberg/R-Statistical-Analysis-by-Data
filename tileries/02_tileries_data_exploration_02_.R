setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)



# ----------
Tl.p <- pdata.frame(Tileries)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

