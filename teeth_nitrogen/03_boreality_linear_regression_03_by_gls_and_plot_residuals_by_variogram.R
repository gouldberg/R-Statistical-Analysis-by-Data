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
# Variogram function from the nlme package, which takes as input the object from a gls, lme, or gamm
# ------------------------------------------------------------------------------

library(nlme)


f1 <- formula(Bor ~ Wet)


B1.gls <- gls(f1, data = Boreality)


summary(B1.gls)



# ----------
# The x and y-coordinates are used to calculate distances (using Pythagoras theorem) between points.
# robust, maxDist:  parameters for calculating the experimental variogram (Cressie, 1993)
Vario.gls <- Variogram(B1.gls, form = ~ x + y, robust = TRUE, maxDist = 2000, resTYpe = "pearson")


Vario.gls



# ----------
plot(Vario.gls, smooth = TRUE)




