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
# Spatial Fixed Effects model
# ------------------------------------------------------------------------------


riceprod0 <- update(riceprod, . ~ . - region - time)


semfemod <- spml(riceprod0, Rice, listw = ricelw, lag = FALSE, spatial.error = "b")



# ----------
summary(semfemod)



# ------------------------------------------------------------------------------
# Hausman-type test
#   - whether the individual effects are toe be treated as fixed or can be assumed incorrelated with regressors,
#     employing a more efficient random effects specification
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


sphtest(riceprod0, Rice, listw = ricelw)



# -->
# Random effects hypothesis being not rejected, random effects metods are in order.



