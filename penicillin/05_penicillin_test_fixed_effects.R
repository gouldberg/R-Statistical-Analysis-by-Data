setwd("//media//kswada//MyFiles//R//penicillin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  penicillin
# ------------------------------------------------------------------------------
data("penicillin", package = "faraway")

str(penicillin)

car::some(penicillin)



# ------------------------------------------------------------------------------
# Test fixed effects by aov() and anova()
# ------------------------------------------------------------------------------

# for this simple balanced model, the aov function can be used
# Random effects term for blend is specified.
amod <- aov(yield ~ treat + Error(blend), data = penicillin)
amod2 <- aov(yield ~ treat, data = penicillin)

summary(amod)
summary(amod2)



# -->
# The p-value for testing the treatment effects is 0.34 indicating no significant effect.


# Also we might try to base a test on the F-statistic.
anova(mmod)



# ------------------------------------------------------------------------------
# Test fixed effects by using adjusted degrees of freedom
#  - More reliable F-tests can be achieved by using adjusted degrees of freedom.
#    The pbkrtest package implements the Kenward-Roger method
#  - This method can be generalized to a much wider class of problems.
# ------------------------------------------------------------------------------

library(pbkrtest)

amod <- lmer(yield ~ treat + (1 | blend), data = penicillin, REML = FALSE)
nmod <- lmer(yield ~ 1 + (1 | blend), data = penicillin, REML = FALSE)

KRmodcomp(amod, nmod)



# -->
# It is essential to use the ML method of estimation when testing fixed effects.
# As can be seen, it produces an identical result to the aov output with the same degrees of freedom and p-value.

