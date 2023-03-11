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
# The Random Intercept and Slope Model by lme (linear mixed effects model)
#   - Suppose that the relationship between species richness and NAP is different on each beach. This implies that we need to include
#     a NAP-Beach interaction term to the model.
#     Because beach has nine levels and one level is used as the baseline, the number of parameters used by this model is excessively high,
#     at 17. And we are not even interested in beach effects !.
#     To estimate model degrees of freedom more efficiently, we can apply the mixed effects model with a random intercept and a random slope.
# ------------------------------------------------------------------------------

RIKZ$fBeach <- factor(RIKZ$Beach)


Mlme2 <- lme(Richness ~ NAP, random = ~1 + NAP | fBeach, data = RIKZ)



summary(Mlme2)



# ----------
AIC(Mlme1);  AIC(Mlme2)


# -->
# Note that the random intercept and slope model has a lower AIC than the earlier models.


# -->
# The amount of variation around the population intercept is 3.54^2 = 12.5
# The estimated variation in slopes at the nine beaches is 1,71^2 = 2.92,
# showing that there is considerably more variation in intercepts than in slopes at the nine beaches.
# There is a correlation between the random intercepts and slopps. Its value of -0.99 is rather high (causing potential numerical problems),
# but indicates that beaches with a high positive intercept also have a high negative slope.
