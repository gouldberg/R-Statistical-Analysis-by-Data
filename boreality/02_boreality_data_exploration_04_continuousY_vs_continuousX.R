# setwd("//media//kswada//MyFiles//R//boreality")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//boreality")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  boreality
# ------------------------------------------------------------------------------

Boreality <- read.table(file = "Boreality.txt", header = TRUE)


str(Boreality)


dim(Boreality)


car::some(Boreality)



# ----------
# Boreality was transformed using the following transformation (See Cressie (p.395, 1993))
# nBor:  the number of species that belong to breal coenosis species
# nTot:  number of all species at the site
Boreality$Bor <- sqrt(1000 * (Boreality$nBor + 1) / (Boreality$nTot))




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X
# ------------------------------------------------------------------------------

plot(Bor ~ Wet, data = Boreality, ylab = "Bor", xlab = "Wet", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Boreality$Wet, Boreality$Bor), col = "blue", lwd = 1)


plot(Bor ~ NDVI, data = Boreality, ylab = "Bor", xlab = "NDVI", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Boreality$NDVI, Boreality$Bor), col = "blue", lwd = 1)


plot(Bor ~ Grn, data = Boreality, ylab = "Bor", xlab = "Grn", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Boreality$Grn, Boreality$Bor), col = "blue", lwd = 1)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(Boreality, aes(x = Wet, y = Bor)) + xlab("Wet") + ylab("Bor") + geom_point(alpha = 0.3) + stat_smooth()

ggplot(Boreality, aes(x = NDVI, y = Bor)) + xlab("NDVI") + ylab("Bor") + geom_point(alpha = 0.3) + stat_smooth()

ggplot(Boreality, aes(x = Grn, y = Bor)) + xlab("Grn") + ylab("Bor") + geom_point(alpha = 0.3) + stat_smooth()



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by scatterplot
# ------------------------------------------------------------------------------

formula <- Bor ~ Wet

car::scatterplot(formula, data = Boreality)



formula <- Bor ~ NDVI

car::scatterplot(formula, data = Boreality)



formula <- Bor ~ Grn

car::scatterplot(formula, data = Boreality)
