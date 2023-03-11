setwd("//media//kswada//MyFiles//R//hedonic")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hedonic
# ------------------------------------------------------------------------------

data("Hedonic", package = "plm")


str(Hedonic)


dim(Hedonic)


car::some(Hedonic)



# ----------
Hp <- pdata.frame(Hedonic, index = "townid")



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

