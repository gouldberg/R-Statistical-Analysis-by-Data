# setwd("//media//kswada//MyFiles//R//ketchup")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//ketchup")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Ketchup
# ------------------------------------------------------------------------------

data("Ketchup", package = "Ecdat")


dim(Ketchup)


str(Ketchup)



car::some(Ketchup)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
