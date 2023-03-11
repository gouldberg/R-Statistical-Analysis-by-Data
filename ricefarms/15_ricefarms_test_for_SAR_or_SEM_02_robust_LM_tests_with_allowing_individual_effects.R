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
# Robust LM tests of SAR or SEM with allowing for individual effects
# ------------------------------------------------------------------------------

local.rob.LMw <- matrix(ncol = 4, nrow = 2)



# dmeaining to subtract time means, thus eliminating any individual effect, of either random or fixed type.
wriceprod <- Within(log(goutput)) ~ Within(log(seed)) + Within(log(totlabor)) + Within(log(size)) + region + time



dimnames(local.rob.LMw) <- list(c("LM test", "p-value"), c("lml", "lme", "rlml", "rlme"))


for(i in c("lml", "lme", "rlml", "rlme")){
  local.rob.LMw[1,i] <- slmtest(wriceprod, data = Rice, listw = ricelw, test = i)$statistic
  
  local.rob.LMw[2,i] <- slmtest(wriceprod, data = Rice, listw = ricelw, test = i)$p.value
}




round(local.rob.LMw, 4)



# -->
# The result is unchanged, but now we are more confident in it 
# because we have controlled, altough in ad hoc way, for individual effects.


