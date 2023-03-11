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
# plot experimental variogram with the fitted spatial correlation
# ------------------------------------------------------------------------------

Vario1E <- Variogram(B1E, form = ~x + y, robust = TRUE, maxDist = 2000, resType = "pearson")


plot(Vario1E, smooth = TRUE)



# ----------
# normalized residulas
Vario2E <- Variogram(B1E, form = ~x + y, robust = TRUE, maxDist = 2000, resType = "normalized")


plot(Vario2E, smooth = TRUE)



# -->
# normalized residuals should no longer show a spatial correlation,
# indicating spatial independence

