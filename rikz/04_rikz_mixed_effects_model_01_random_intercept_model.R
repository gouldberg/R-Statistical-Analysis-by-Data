# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# The Random Intercept Model by lme (linear mixed effects model)
# ------------------------------------------------------------------------------

RIKZ$fBeach <- factor(RIKZ$Beach)


# ~1 | fBeach specifies a random intercept model.
Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach, data = RIKZ)


summary(Mlme1)


# -->
# the residual variance is estimated as 3.05^2 = 9.30 and
# and the variance for the random intercept is estimated as 2.94^2 = 8.64.


