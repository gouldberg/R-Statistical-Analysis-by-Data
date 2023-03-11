setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ----------
Rice <- pdata.frame(RiceFarms, index = "id")



# ------------------------------------------------------------------------------
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------

# Variance decomposition:  "log(goutput)" is dependent variable in this example
summary(log(Rice$goutput))



# -->
# The variation is mainly due to inter-individual (64.4%)





