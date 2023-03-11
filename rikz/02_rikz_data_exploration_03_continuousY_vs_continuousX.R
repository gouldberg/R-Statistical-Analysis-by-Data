# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X
# ------------------------------------------------------------------------------

plot(Richness ~ NAP, data = RIKZ, ylab = "Richness", xlab = "NAP", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(RIKZ$Richness, RIKZ$NAP), col = "blue", lwd = 1)


plot(Richness ~ Exposure, data = RIKZ, ylab = "Richness", xlab = "Exposure", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(RIKZ$Richness, RIKZ$Exposure), col = "blue", lwd = 1)



# -->
# It seems that this is not best way to show the relationship for this data



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(RIKZ, aes(x = NAP, y = Richness)) + xlab("NAP") + ylab("Richness") + geom_point(alpha = 0.3)




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by scatterplot
# ------------------------------------------------------------------------------

formula <- Richness ~ NAP

car::scatterplot(formula, data = RIKZ)

