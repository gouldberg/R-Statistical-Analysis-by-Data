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
# The Random Effects Model
#   - A linear mixed effects model that does not contain any beta, except for an intercept is called a random effects model.
#   - The model implies that richness is modelled as an intercept plus a random term, that is allowed to differ per beach.
# ------------------------------------------------------------------------------

RIKZ$fBeach <- factor(RIKZ$Beach)


Mlme3 <- lme(Richness ~ 1, random = ~1 | fBeach, data = RIKZ)


summary(Mlme3)



