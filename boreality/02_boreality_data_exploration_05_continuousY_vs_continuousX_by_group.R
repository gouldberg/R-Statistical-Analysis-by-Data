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
# data exploration:  continuous Y vs. continuous X  by group   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(Boreality, aes(Wet, Bor)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "Bor", x = "Wet")


gg + facet_wrap(~ Oxalis)



# ----------
gg <- ggplot(Boreality, aes(NDVI, Bor)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "Bor", x = "NDVI")


gg + facet_wrap(~ Oxalis)



# ----------
gg <- ggplot(Boreality, aes(Grn, Bor)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "Bor", x = "Grn")


gg + facet_wrap(~ Oxalis)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by coplot
# ------------------------------------------------------------------------------

formula = Bor ~ Wet | Oxalis

coplot(formula, data = Boreality, ylab = "Bor", xlab = "Wet", las=1)


formula = Bor ~ NDVI | Oxalis

coplot(formula, data = Boreality, ylab = "Bor", xlab = "NDVI", las=1)


formula = Bor ~ Grn | Oxalis

coplot(formula, data = Boreality, ylab = "Bor", xlab = "Grn", las=1)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

formula = Bor ~ Wet | Oxalis

xyplot(formula, data = Boreality, type = c("p", "g", "smooth"))




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by scatterplot
# ------------------------------------------------------------------------------

formula <- Bor ~ Wet | Oxalis

scatterplot(formula, data = Boreality, col = c("black", "darkgray"))



formula <- Bor ~ NDVI | Oxalis

scatterplot(formula, data = Boreality, col = c("black", "darkgray"))



formula <- Bor ~ Grn | Oxalis

scatterplot(formula, data = Boreality, col = c("black", "darkgray"))

