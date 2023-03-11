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
# Convert a square spatical weights matrix to a weights list object
# ------------------------------------------------------------------------------

library(spdep)


# convert a square spatial weights matrix to a weights list object
ricelw <- mat2listw(riceww)

ricelw



# ----------
ricelw$style

ricelw$neighbours

str(ricelw$weights)



# ------------------------------------------------------------------------------
# Spatial Error Model (SEM) 
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


pdim(Rice)


index(Rice)



# ----------
# With respect to the original analysis, our production frontier equation will relate rice output to three inputs only:
# seed, labor hours totlabor, and land size, all in logs
# The SEM panel model is explicitly augmented with village fixed effects and time fixed effects to acount for the influence of the different growing seasons/

riceprod <- log(goutput) ~ log(seed) + log(totlabor) + log(size) + region + time


library(splm)

rice.sem <- spreml(riceprod, data = Rice, w = riceww, lag = FALSE, errors = "sem")


summary(rice.sem)



# -->
# somewhat surprisingly, the village fixed effects show up as all but unimportant.
# On the converse, spatial error corrleation between farms belonging to the same village (estiamted by "rho" = 0.563)
# is substantial and highly significant




