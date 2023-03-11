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
# F tests for individual and/or time effects
#   - The test of the null hypothesis of no individual and time effects:  two-ways within model vs pooling model
#   - The test of the null hypothesis of no individual effects:  within model vs. pooling model
#   - ...
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


# ----------
pdim(Rice)


index(Rice)



# ----------
rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)



# ----------
rice.w <- plm(rice.eq, data = Rice, model = "within")


rice.p <- update(rice.w, model = "pooling")


rice.wd <- plm(rice.eq, data = Rice, effect = "twoways")



# ----------
# The test of the null hypothesis of no individual effects

pFtest(rice.w, rice.p)


# -->
# Unsurprisingly, the absence of individual effects is strongly rejected.



# ----------
# The test of the null hypothesis of no individual and no time effects

pFtest(rice.wd, rice.p)



# ----------
# The test of the null hypothesis of no time effects allowing for the presence of individual effects

pFtest(rice.wd, rice.w)




