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
# Data Exploration:  Visualize spatial location
# ------------------------------------------------------------------------------

xyplot(y ~ x | Oxalis, aspect = "iso", col = 1, 
       data = Boreality, xlab = "X-coordinate",  ylab = "Y-coordinate")



xyplot(y ~ x, 
       aspect = "iso", 
       groups = Boreality$Oxalis,
       data = Boreality, xlab = "X-coordinate",  ylab = "Y-coordinate", col = 1:2, pch = c(1,20), cex = 0.7)



# ------------------------------------------------------------------------------
# Data Exploration:  Visualize spatial location with bubble plot way
# ------------------------------------------------------------------------------

# library(gstat)
library(sp)


# substract mean from Bor, since bubble function plots negative values as first color and positive values as second color
mydata <- data.frame(mBor = Boreality$Bor - mean(Boreality$Bor), Boreality$x, Boreality$y)

coordinates(mydata) <- c("Boreality.x", "Boreality.y")


bubble(mydata, "mBor", col = c(gray(0.7), "black"), main = "Bor", xlab = "X-coordiantes", ylab = "Y-coordinates")
