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
# Robust LM tests of SAR or SEM
#   - The standard versions of the (pooled) LM test for SAR (SEM), as observed, is not robust to the presence of a SEM (SAR)
#     term, i.e., of the "other" effect.
#     Robust LM tests instead allow for "local" deviations from zero of the "other" parameter.
#     Still, the extent of the tolerated deviation is uncertain.
# ------------------------------------------------------------------------------


local.rob.LM <- matrix(ncol = 4, nrow = 2)


# lml:  non-robust SAR test
# rlml:  locally robust SAR test
tests <- c("lml", "lme", "rlml", "rlme")


dimnames(local.rob.LM) <- list(c("LM test", "p-value"), tests)


for(i in tests){
  local.rob.LM[1,i] <- slmtest(riceprod, data = Rice, listw = ricelw, test = i)$statistic
  
  local.rob.LM[2,i] <- slmtest(riceprod, data = Rice, listw = ricelw, test = i)$p.value
}



round(local.rob.LM, 4)



# -->
# The differnce between the false positive given by the non-robust SAR test and the locally robust counterpart clearly stands out
# Robust test favors the SEM model over the SAR.


