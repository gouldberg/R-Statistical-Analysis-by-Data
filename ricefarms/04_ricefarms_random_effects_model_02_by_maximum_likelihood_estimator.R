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
# Random Effects Model by Maximum Likelihood Estimator
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


# ----------
pdim(Rice)


index(Rice)



# ----------
Q.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)


library(pglm)


# Estimate the Swamy and Arora model
rice.ml <- pglm(Q.eq, data = Rice, family = gaussian)


summary(rice.ml)

summary(Q.swar)


# -->
# The coefficients are very similar to those obtained with the GLS estimator.

# The 2 parameters sd.idios and sd.id are the estimated standard deviations of the idiosyncratic and of the individual parts of the error
# Those too are almost close to GLS estimator.


