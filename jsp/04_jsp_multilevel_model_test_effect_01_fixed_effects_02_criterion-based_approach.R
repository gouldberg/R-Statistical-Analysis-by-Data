setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------

data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)



# ------------------------------------------------------------------------------
# Test fixed effects by criterion-based approach
#  - In this example, we have more than a handful of potential models we might consider even if we vary only
#    the fixed effect part of the model.
#    In such circumstances, we might prefer to take a criterion-based approach to model selection.
# ------------------------------------------------------------------------------

# Since we take criterion-based approach, set REML = FALSE (We use ML)
# It is not sensible to use the REML method when comparing models with different fixed effects.

# We have specified models with a 3-way interaction, all 2-way interactions, models leaving out each 2-way interaction, a model excluding any interaction
# involving gender, a model with just main effects and finally a model without gender entirely.

all3 <- lmer(math ~ raven * social * gender + (1 | school) + (1 | school : class), data = jspr, REML = FALSE)


all2 <- update(all3, . ~ . - raven:social:gender)
notrs <- update(all2, . ~ . - raven:social)
notrg <- update(all2, . ~ . - raven:gender)
notsg <- update(all2, . ~ . - social:gender)
onlyrs <- update(all2, . ~ . - social:gender - raven:gender)


all1 <- update(all2, . ~ . - social:gender - raven:gender - social:raven)
nogen <- update(all1, . ~ . - gender)



# ----------
# AIC and BIC
anova(all3, all2, notrs, notrg, notsg, onlyrs, all1, nogen, sortby = "BIC")[1:4]



# -->
# Not that the anova output produces chi-squared tests for comparing the models.
# This is not correct here as the sequence of models is not nested and furthermore, these tests are inaccurate for reasons previously explained.

# We can see that the AIC is minimized by the model that "onlyrs" (gender is included as main effect only)
# The BIC criterion commomly preferes models that are smaller than the AIC, minimized by the model that removes gender entirely ("nogen")



# ----------
anova(onlyrs)
anova(nogen)

summary(nogen)


