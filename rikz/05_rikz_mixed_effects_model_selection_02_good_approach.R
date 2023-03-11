setwd("//media//kswada//MyFiles//R//rikz")

packages <- c("dplyr", "lattice", "nlme")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------
RIKZ <- read.table(file = "//media//kswada//MyFiles//references//ZuurDataMixedModelling//RIKZ.txt", header = TRUE)


str(RIKZ)


RIKZ$fBeach <- factor(RIKZ$Beach)



# ------------------------------------------------------------------------------
# model selection:  good approach
#   - We should start with as many explanatory variables as possible in the fixed component.
# ------------------------------------------------------------------------------

B1 <- gls(Richness ~ 1 + NAP * fExp, data = RIKZ, method = "REML")

B2 <- lme(Richness ~ 1 + NAP * fExp, data = RIKZ, random = ~ 1 | fBeach, method = "REML")

B3 <- lme(Richness ~ 1 + NAP * fExp, data = RIKZ, random = ~ 1 + NAP | fBeach, method = "REML")



# ----------
# The random intercept model is preferred option
AIC(B1, B2, B3)

# We can also use the likelihood ratio test via the anova command as the models are nested.
anova(B1, B2, B3)



# ----------
# all parameters in this model are significant
summary(B2)



# ----------
# But a p-value of 0.04 is unconvincing, we could drop the interaction and refit the model
# Drop interaction
B2 <- lme(Richness ~ 1 + NAP + fExp, data = RIKZ, random = ~ 1 | fBeach, method = "REML")

summary(B2)



# -->
# Note that we end up with a fundamentally different model compared to "Wrong Approach".
# The reason we ended up with a different model is because in the previous example, part of the information that we want to have in the fixed effects
# ended up in the random effects.

