setwd("//media//kswada//MyFiles//R//parzen")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: parzen
#   - Variables:
#        - snowfall:  the annual snowfall in Buffalo, NY (inches) from 1910 to 1972 inclusive
# ------------------------------------------------------------------------------

data("parzen", package = "gamlss.data")


str(parzen)

car::some(parzen)



# ------------------------------------------------------------------------------
# Fit nonparametric density
# ------------------------------------------------------------------------------

graphics.off()

MASS::truehist(parzen$snowfall, nbins = 25, col = "grey", xlab = "Snowfall", ylab = "Density", ylim = c(0, 0.05))

lines(histSmo(parzen$snowfall), lty = 1, lwd = 2)

lines(histSmoC(parzen$snowfall, df = 7), lty = 2, lwd = 2)

legend("topright", legend = c("local ML", "fixed df"), lty = 1:2, cex = 1.0)


