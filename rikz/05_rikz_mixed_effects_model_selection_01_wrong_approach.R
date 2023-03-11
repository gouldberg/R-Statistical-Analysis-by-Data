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
# model selection:  wrong approach
#   - We show the danger of not starting with a full model.
#     To illustrate this, we take NAP as the only fixed explanatory variable for the fixed component and ignore exposure for the moment.
# ------------------------------------------------------------------------------

Wrong1 <- gls(Richness ~ 1 + NAP, method = "REML", data = RIKZ)

Wrong2 <- lme(Richness ~ 1 + NAP, random = ~ 1 | fBeach, method = "REML", data = RIKZ)

Wrong3 <- lme(Richness ~ 1 + NAP, method = "REML", random = ~ 1 + NAP | fBeach, data = RIKZ)



# ----------
# Because REML estimation was used, we can compare AICs and BICs.
AIC(Wrong1, Wrong2, Wrong3)



# -->
# This suggests the model with the random intercept and slope is the best.



# ----------
# We can also use the likelihood ratio test via the anova command as the models are nested.
anova(Wrong1, Wrong2, Wrong3)



# -->
# These models are nested with respect to the variances.
# Unfortunately, there is a little problem here, which is the 'testing on the boundary'.
# The null hypothesis of this test is H0: sigma^2 = 0 versus the alternative H1: sigma^2 > 0
# The p-value provided by the anova function is incorrect as this function assumes that twice the differences between the two log-likelihood values
# follows a Chi-square distribution with p degrees of freedom.
# However, when testing on the boundary, p-value should be divided by 2
# Note that this correction only applies for comparing a model without and with a random intercept !

0.5 * (1 - pchisq(-2 * (-126.10 + 119.74), 1))


# If we want to compare the model with the random intercept and the model with random intercept and slope
0.5 * ((1 - pchisq(7.09, 1)) + (1 - pchisq(7.09, 2)))



# ----------
summary(Wrong3)



# -->
# The slope for NAP is significant. Hence, dropping NAP from the model is not an option.
# The only thing we can try is adding exposure or adding exposure and the interaction between exposure and NAP.



# ----------
RIKZ$fExp <- RIKZ$Exposure

RIKZ$fExp[RIKZ$fExp == 8] <-10

RIKZ$fExp <- factor(RIKZ$fExp, levels=c(10,11))

Wrong4 <- lme(Richness ~1 + NAP * fExp, random = ~1 + NAP | fBeach, method = "REML", data = RIKZ)

anova(Wrong4)



# -->
# The anova command applies sequential testing; the interaction term is the last term to be added.
# The p-value suggests it is not significant at the 5% level.



# ----------
summary(Wrong4)



# -->
# The t-statistic also shows that we can drop the interaction term.
# Drop the interaction

Wrong4 <- lme(Richness ~1 + NAP + fExp, random = ~1 + NAP | fBeach, method = "REML", data = RIKZ)

summary(Wrong4)



# -->
# These tests are approximate, meaning that we should not take them too literally, hence, p = 0.04 is not convincing
# evidence of an exposure effect.

# Note for degrees of freedom:
# For a level 2 variable (an explanaatory variable that has the same value for all observations within the levels of the random effect)
# the degrees of freedom is calculated as the number of level 2 clusters (=9 levels in the random variable beach) minus
# the number of level 2 fixed variables (in case only exposure) minus 1 if there is an intercept --> 7 for exposure.


 
# ----------
# The likelihood ratio test using ML estimation
# To avoid an error message related to convergence, we used the control option.
lmc <- lmeControl(niterEM = 5200, msMaxIter = 5200)

Wrong4A <- lme(Richness ~ 1 + NAP, method = "ML", control = lmc, data = RIKZ, random = ~ 1 + NAP | fBeach)
Wrong4B <- lme(Richness ~ 1 + NAP + fExp, random = ~1 + NAP | fBeach, method = "ML", data = RIKZ, control = lmc)
Wrong4C <- lme(Richness ~ 1 + NAP * fExp, random = ~1 + NAP | fBeach, data = RIKZ, method = "ML", control = lmc)

anova(Wrong4A, Wrong4B, Wrong4C)


# -->
# Optimal model contains NAP as a fixed effect with a random slope and intercept (borderline significant p = 0.04)



# ----------
Wrong5 <- lme(Richness ~ 1 + NAP, random = ~ 1 + NAP | fBeach, method = "REML", data = RIKZ)

summary(Wrong5)
