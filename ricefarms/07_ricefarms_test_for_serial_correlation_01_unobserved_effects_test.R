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
# Tests for Serial Correlation:  Unobserved Effects Test  (Wooldridge)
#   - semi-parametric test for the null hypothesis that sigma(ega)^2 i.e., that there are no unobserved effects in the residuals
#   - The test statistic is (N-) asymptotically distributed as a standard normal regardless of the distribution of the errors.
#     It does also not rely on homoscedasticity.
#   - It has power both against the standard random effects specification, where the unobserved effects are constant within every group,
#     as well as against any kind of serial correlation.
#     As such it "nests" both individual effects and serial correlation tests, trading some power against more specific alternatives in exchange for robustness.
# ------------------------------------------------------------------------------


Rice <- pdata.frame(RiceFarms, index = "id")


# ----------
pdim(Rice)


index(Rice)



# ----------
rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)



# ----------
pwtest(rice.eq, Rice)


# -->
# The null hypothesis of no unobserved effects IS REJECTED !!!

