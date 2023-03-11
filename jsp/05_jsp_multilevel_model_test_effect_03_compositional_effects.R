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
# We shall take as our response the math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)



# ----------
jspr$craven <- jspr$raven - mean(jspr$raven)

mmod_final <- lmer(math ~ craven * social + (1 | school) + (1 | school : class), data = jspr)



# ------------------------------------------------------------------------------
# Test compositional effects
#   - It is not improbable that factors at the school or class level might be important predictors of success in the math test.
#     We can construct some such predictors from the individual-level information;  such factors are called compositional effects.
#     The ability of one's fellow students may have an impact on future achievement.
# ------------------------------------------------------------------------------

# Average entering score for a school might be an important predictor.
schraven <- lm(raven ~ school, data = jspr)$fit


mmodc <- lmer(math ~ craven * social * schraven * social + (1 | school) + (1 | school : class), data = jspr)


KRmodcomp(mmod_final, mmodc)



# -->
# We see that this new effect is not significant.
# We are not constrained to taking means. We might consider various quantiles or measures of spread as potential compositional variables.


