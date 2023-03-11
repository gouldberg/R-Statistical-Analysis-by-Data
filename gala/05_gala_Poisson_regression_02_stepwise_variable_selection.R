setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# Stepwise variable selection
# ------------------------------------------------------------------------------

modp <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + log(Adjacent) + log(Scruz + 0.1), data = gala, family=poisson(link="log"))


library(MASS)

modp.step1 <- stepAIC(modp, trace = TRUE, direction = "both")


modp.step1$anova



# ---------
# Alternatively we can use the BIC criterion, which generally will select a smaller model when the sample size is reasonably large

modp.step2 <- stepAIC(modp, trace = TRUE, direction = "both", k = log(nrow(gala)))


modp.step2$anova



# -->
# log(Elevation) is removed in both selecitons



# ------------------------------------------------------------------------------
# For trial, we remove Elevation, Nearest, Scruz !
# ------------------------------------------------------------------------------

modp2 <- glm(Species ~ log(Area) + log(Adjacent), data = gala, family=poisson(link="log"))


