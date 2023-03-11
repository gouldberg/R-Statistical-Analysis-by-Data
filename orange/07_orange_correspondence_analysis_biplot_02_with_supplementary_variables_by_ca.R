setwd("//media//kswada//MyFiles//R//orange")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orange
# ------------------------------------------------------------------------------

orange <- read.table("orange.csv", header=TRUE, sep=";", dec=".", row.names=1)

str(orange)

dim(orange)


orange



# ------------------------------------------------------------------------------
# Correspondence Analysisis Biplot with supplementary variables
# ------------------------------------------------------------------------------

library(ca)


fit_ca <- ca(orange[,1:14], supcol = 8:14)


graphics.off()
par(mfrow = c(1,1))
plot(fit_ca, arrows = c(FALSE, TRUE))



# ----------
fit_ca


summary(fit_ca)


# ----------
fit_ca$sv

# sqrt(fit_anass$eigen.values)


# -->
# The singular values for the first two dimensions are 0.1435 and 0.0568
# They do differ ...

