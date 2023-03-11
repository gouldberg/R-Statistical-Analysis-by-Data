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
# Item Characteristic Curves (ICCs)
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2), mar = c(2,2,2,2))

plotICC(fitrsm2, ask = TRUE, col = c("black", "blue", "red"), lty = 1:3, ylab = "")




# ------------------------------------------------------------------------------
# Item Characteristic Curves (ICCs):  Threshold
# ------------------------------------------------------------------------------

# The threshold parameters reflect the point on the empathy trait continuum
# where a respondent would switch from scoring 0 to scoring 1 and would switch from scoring 1 to scoring 2

thpar <- thresholds(fitrsm2)


thpar



# ----------
# Note that the each range is same  --> Rating Scale Model

data.frame(thpar$threshtable$'1') %>% mutate(dif1 = Location - Threshold.1, dif2 = Threshold.2 - Location)




# ------------------------------------------------------------------------------
# Compare ceaq9 and ceaq14
# ------------------------------------------------------------------------------


par(mfrow = c(1,1), ask = FALSE)

plot(prin)



# ----------
( th9 <- thpar$threshtable$'1'["ceaq9",] )

( th14 <- thpar$threshtable$'1'["ceaq14",] )




par(mfrow = c(2,1))

plotICC(fitrsm2, item.subset = "ceaq9", col = c("black", "blue", "red"), lty = 1:3, lwd = 2, ylab = "")

abline(v = c(th9[1], th9[2], th9[3]), lty = 2, col = "darkgray")

plotICC(fitrsm2, item.subset = "ceaq14", col = c("black", "blue", "red"), lty = 1:3, lwd = 2,ylab = "")

abline(v = c(th14[1], th14[2], th14[3]), lty = 2, col = "darkgray")



# -->
# Note that each item has different discrimination range for latent dimension

