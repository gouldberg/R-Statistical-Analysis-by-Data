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
# Multilevel model
#   - Fixed effects:  main effects + all interactions between raven, social and gender
#   - Random effects:  school + class nested within school
# ------------------------------------------------------------------------------

table(jspr$school)


# -->
# In ordinary linear regression, we are assuming that the 953 students in the dataset are independent observations.
# This is not a tenable assumption as the students come from 50 different schools.

# It is highly likely that students in the same school (and perhaps class) will show some dependence.
# So we have somewhat less than 953 independent cases worth of information.



# ----------
# Fixed effects representing all interactions between raven, social and gender
# with random effects for the school and the class nested within the school.

library(lme4)

mmod <- lmer(math ~ raven * social * gender + (1 | school) + (1 | school : class), data = jspr)


summary(mmod)


car::Anova(mmod)



# -->
# This model suggests that gender may not be significant.



# ----------
confint(mmod, method = "boot")

