setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compaatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho



# ----------
covars <- CEAQ %>% dplyr::select(age, grade, gender)




# ------------------------------------------------------------------------------
# test parameter instability at each node
# ------------------------------------------------------------------------------

library("strucchange")


sctest(rstr, node = 4)


sctest(rstr, node = 6)


sctest(rstr, node = 8)


sctest(rstr, node = 9)


