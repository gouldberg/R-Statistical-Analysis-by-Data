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
# linear regression:  raven * gender * social
# ------------------------------------------------------------------------------


glin <- lm(math ~ raven * social * gender, data = jspr)

summary(glin)



# ----------
# Analysis of Variance Table by each term (not by each level)
anova(glin)


# -->
# gender is not significant



# ------------------------------------------------------------------------------
# linear regression:  raven * social
# ------------------------------------------------------------------------------


glin2 <- lm(math ~ raven * social, data = jspr)

anova(glin2)



# -->
# This is a fairly large dataset, so even small effects can be significant, Even though the raven:social term is significant at the 5% level,
# we remove it to simplify interpretation.




# ------------------------------------------------------------------------------
# linear regression:  raven + social
# ------------------------------------------------------------------------------

glin3 <- lm(math ~ raven + social, jspr)

summary(glin3)


anova(glin3)



# -->
# We see that the final math score is strongly related to the entering Raven score and
# that the math scores of the lower social classes are lower,
# even after adjustment for the entering score.




# ------------------------------------------------------------------------------
# compare models
# ------------------------------------------------------------------------------

AIC(glin, glin2, glin3)



# -->
# glin2 (no gender model but interaction model) is best 

