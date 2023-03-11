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
# model comparison
# ------------------------------------------------------------------------------

AIC(B1.gls, B1A, B1C, B1D, B1E)



# -->
# AIC of the model with no correlation is 2844.541, but the model with spatial correlation have considerable lower AIC values
# B1E (corExp structure) is the best in terms of AIC



# ----------
anova(B1.gls, B1E)



# -->
# adding a spatial correlation structure gives a significantly better model.

