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
# Linear Regression
# ------------------------------------------------------------------------------

B.lm <- lm(Bor ~ Wet, data = Boreality)


summary(B.lm)



# ------------------------------------------------------------------------------
# Plot variogram of standardized residuals  (isotropy assumption)
# ------------------------------------------------------------------------------

E <- rstandard(B.lm)


library(gstat)
library(sp)


mydata <- data.frame(E, Boreality$x, Boreality$y)

coordinates(mydata) <- c("Boreality.x", "Boreality.y")



# ----------
Vario1 <- variogram(E ~ 1, mydata)

plot(Vario1)



# -->
# There is a clear spatial correlation up to about 1000m
# There is also a nugget effect of approximately 0.75.

# Note that this variogram assumes isotropy: the strength of the spatial correlation is the same in each direction



# ------------------------------------------------------------------------------
# Plot variogram of standardized residuals:  by multiple direction
# ------------------------------------------------------------------------------

Vario2 <- variogram(E ~ 1, mydata, alpha = c(0, 45, 90, 135))

plot(Vario2)



# -->
# It seems that isotropy is a reasonable assumption as the strength, and pattern,
# of the spatial correlation seems to be broadly the same in all four directions





