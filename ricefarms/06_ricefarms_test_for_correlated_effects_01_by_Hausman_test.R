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
# Hausman test for correlated effects
#   - H0:  no correlation between the regressors and the individual effects
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


# ----------
pdim(Rice)


index(Rice)



# ----------
rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)



# ----------
rice.w <- plm(rice.eq, data = Rice, model = "within")


rice.r <- update(rice.w, model = "random")



# ----------
# The test of the null hypothesis of no correlation between the regressors and the individual effects
# (the statistic is distributed as a X^2 with 3 degrees of freedom)

phtest(rice.w, rice.r)



# -->
# Not rejected, indicating NOT that there are some correlation

