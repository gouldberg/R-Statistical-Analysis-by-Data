setwd("//media//kswada//MyFiles//R//glasses")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glasses
#   - The age at which participants in the Blue Mountains Eye Study reported that they started wearing reading glasses.
#   - Variables:
#        - age:  age of participants
#        - sex:  1: male  2: female
#        - ageread:  age at which participant started wearing reading glasses
# ------------------------------------------------------------------------------
data("glasses", package = "gamlss.data")


str(glasses)

car::some(glasses)



# ------------------------------------------------------------------------------
# Fit nonparametric density
# ------------------------------------------------------------------------------

MASS::truehist(glasses$ageread, nbins = 25, col = "grey", xlab = "Age", ylab = "Density", ylim = c(0, 0.05))

lines(histSmo(glasses$ageread), lty = 1, lwd = 2)

lines(histSmoC(glasses$ageread, df = 7), lty = 2, lwd = 2)

legend("topleft", legend = c("local ML", "fixed df"), lty = 1:2, cex = 1.5)



# -->
# The histogram and the two density estimates are suggestive of two subpopulations of subjects:
# those who started wearing reading glasses in childhood and early adulthood, and those who started wearing them later in life.
