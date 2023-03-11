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
# Test fixed effects by using adjusted degrees of freedom (Kenward-Roger adjusted F-test)
#  - More reliable F-tests can be achieved by using adjusted degrees of freedom.
#    The pbkrtest package implements the Kenward-Roger method
#  - This method can be generalized to a much wider class of problems.
# ------------------------------------------------------------------------------

library(pbkrtest)


# with gender
mmod <- lmer(math ~ raven * social * gender + (1 | school) + (1 | school : class), data = jspr)


# without gender
mmodr <- lmer(math ~ raven * social + (1 | school) + (1 | school : class), data = jspr)



# ----------
KRmodcomp(mmod, mmodr)



# -->
# It is essential to use the ML method of estimation when testing fixed effects.
# The test does not reject, indicating that gender effect is not significant.

# The size of the dataset means that we can be quite confident about the adjusted F-test in any case.

