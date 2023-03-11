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
# Final model
# ------------------------------------------------------------------------------

# Given that we have decided that gender is not important, we simplify
# We centered the Raven score about its overall mean. This means that we can interpret the social effects as the predicted differences from
# social class one at the mean Raven score.
# If we did not do this, these parameter estimates would represent differences for raven = 0 which is not very useful.

jspr$craven <- jspr$raven - mean(jspr$raven)

mmod_final <- lmer(math ~ craven * social + (1 | school) + (1 | school : class), data = jspr)

summary(mmod_final)



# -->
# We can see that for the scaled entering score, the final math score tends to be lower as social class goes down.
# Note that class 9 here is when the father is absent and class 8 is not necessarily worse than 7, so this factor is not entirely ordinal.
# We also see the most substantial variation at the individual level with smaller amounts of variation at the school and class level.



# ----------
mod_obj <- mmod_final



# ------------------------------------------------------------------------------
# Model disgnostics:  Check the assumption of normality distributed random effects
# ------------------------------------------------------------------------------

qqnorm(ranef(mod_obj)$school[[1]], main = "school effects")

qqline(ranef(mod_obj)$school[[1]])



# ----------
qqnorm(ranef(mod_obj)$'school:class'[[1]], main = "school:class effects")

qqline(ranef(mod_obj)$'school:class'[[1]])
