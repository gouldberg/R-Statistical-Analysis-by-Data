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



# ------------------------------------------------------------------------------
# goodness of fit
# ------------------------------------------------------------------------------

# eliminate item 10 and refit

obj_item <- "ceaq10"


ind <- match(obj_item, colnames(itceaq))


itceaq1 <- itceaq[,-ind]


fitrsm1 <- RSM(itceaq1)


ppar1 <- person.parameter(fitrsm1)


ifit1 <- eRm::itemfit(ppar1)


ifit1



# ----------
# eliminate item 15 and refit again

obj_item <- "ceaq15"


ind <- match(obj_item, colnames(itceaq1))


itceaq2 <- itceaq1[,-ind]


fitrsm2 <- RSM(itceaq2)


ppar2 <- person.parameter(fitrsm2)


ifit2 <- eRm::itemfit(ppar2)


ifit2



# -->
# There is no item left which shows a suspicious combination of significant p-values and extreme infit/outfit values

# (but here ceaq4 is bad ...)



