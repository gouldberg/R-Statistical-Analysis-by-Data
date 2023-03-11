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
# Person Item Map
# ------------------------------------------------------------------------------

plotPImap(fitrsm2, latdim = "Empathy", main = "Person-Item Map CEAQ")


# -->
# solid dots:  the item location parameters
# hollow dots:  the thretholds parameters



# ----------
# for reference:  thresholds for ceaq9 and ceaq14

thpar <- thresholds(fitrsm2)

( th9 <- thpar$threshtable$'1'["ceaq9",] )

( th14 <- thpar$threshtable$'1'["ceaq14",] )


