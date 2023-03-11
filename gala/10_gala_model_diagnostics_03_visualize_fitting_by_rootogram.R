setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# Visualize fitting by rootogram
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,1))

countreg::rootogram(modp, max = 300, main = "Poisson")

countreg::rootogram(mod.nbin, max = 300, main = "Negative Binomial")

