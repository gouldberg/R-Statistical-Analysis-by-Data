setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# Correspondence Analysisis Biplot:  Symmetric and Asymmetric CA map
# ------------------------------------------------------------------------------

library(anacor)


# scaling: first one corresponds to the method for row scaling, the second for column scaling
fit_anass <- anacor(temperature[1:23,1:12], scaling = c("standard", "standard"))

fit_anasb <- anacor(temperature[1:23,1:12], scaling = c("standard", "Benzecri"))

fit_anabs <- anacor(temperature[1:23,1:12], scaling = c("Benzecri", "standard"))

fit_anabb <- anacor(temperature[1:23,1:12], scaling = c("Benzecri", "Benzecri"))



graphics.off()
par(mfrow = c(2,2))
plot(fit_anass, main = "Symmetric CA Map", asp = NULL, arrows = c(FALSE, TRUE))
plot(fit_anasb, main = "Asymmetric CA Map", asp = NULL, arrows = c(FALSE, TRUE))
plot(fit_anabs, main = "Asymmetric CA Map", asp = NULL, arrows = c(FALSE, TRUE))
plot(fit_anabb, main = "Symmetric CA Map", asp = NULL, arrows = c(FALSE, TRUE))




# ----------
fit_anass$eigen.values
fit_anabb$eigen.values

sqrt(fit_anass$eigen.values)



# -->
# The singular values for the first two dimensions are 1 and 0.008

