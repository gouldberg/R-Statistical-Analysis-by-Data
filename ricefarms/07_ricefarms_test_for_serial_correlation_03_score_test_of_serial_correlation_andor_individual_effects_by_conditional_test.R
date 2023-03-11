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
# Tests for Serial Correlation:  Score test 
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Conditional test of Baltagi and Li for serial correlation
#   - testing the null hypothesis of no serial correlation, the hypothesis of the presence of individual effects being maintained
#     under both the null and the alternative hypothesis
#   - This test allow for random effects of any magnitude
#   - pbltest function uses residuals of the random effects maximum likelihood estimator
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


# ----------
pdim(Rice)


index(Rice)



# ----------
rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)



# ----------
pbltest(rice.eq, Rice, alternative = "onesided")



# -->
# Serial correaltion is detected
