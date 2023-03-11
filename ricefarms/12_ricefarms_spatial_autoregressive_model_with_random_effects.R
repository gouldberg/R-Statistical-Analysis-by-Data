setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# data:  riceww
#  - contiguity matrix where for each farm all other farms from the same village are defined as neighbors
# ------------------------------------------------------------------------------

data("riceww", package = "splm")


str(riceww)

dim(riceww)




# ------------------------------------------------------------------------------
# Spatial autoregressive model with random effects (SARRE)
# ------------------------------------------------------------------------------


sarremodml <- spml(riceprod0, Rice, listw = ricelw, model = "random", lag = TRUE, spatial.error = "none")


summary(sarremodml)



# -->
# Here for illustrative purpose.
# This specification has little economic underpinning, there not being many reasons why the output of one farm
# should depend on the output of neighboring ones (here, firms from the same village)

# neverthless, the spatial autoregreeeive model parameter lambda turns out significant and relatively large in magnitude.
# This will proce to be a feature of model specificaiton, more precisely of neglecting the "true" source of spatial dependence (SEM term)



