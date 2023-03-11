setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)


TV2 <- margin.table(TV, c(1,3))

TV2



# ------------------------------------------------------------------------------
# Correspondence Analysisis Biplot:  Symmetric and Asymmetric CA map
# ------------------------------------------------------------------------------

library(anacor)


# scaling: first one corresponds to the method for row scaling, the second for column scaling
fit_anass <- anacor(TV2, scaling = c("standard", "standard"))

fit_anasb <- anacor(TV2, scaling = c("standard", "Benzecri"))

fit_anabs <- anacor(TV2, scaling = c("Benzecri", "standard"))

fit_anabb <- anacor(TV2, scaling = c("Benzecri", "Benzecri"))



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
# The singular values for the first two dimensions are 0.2862 and 0.1025
# They do differ ...

