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
# Chamberlain approach for test for correlated effects
#   - more general model than that of Mundlak
#     In his model, the individual effects are not assumed to be a linear function of the means of the explanatory variables anymore,
#     but of their values over the whole time period
# ------------------------------------------------------------------------------


rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)



# ----------
piest(rice.eq, data = RiceFarms, index = "id")


# -->
# Note that Chi-sq statistics and df are different from Mundlak approach



# ----------
# The variant of the Chamberlain test proposed by Angrist and newey (1991)
aneweytest(rice.eq, data = RiceFarms, index = "id")



# -->
# The restrictions implied by the "within" model are rejected by both tests at the 5% level,
# although they are not rejected at the 1% level for Chamberlain's version of the test

