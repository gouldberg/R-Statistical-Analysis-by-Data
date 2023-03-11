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
# The collection of index plot
#   - for mixed-effects models, influence() refits the model deleting each of group in turn,
#     and provide deletion statistics
#   - influenceIndexPlot() create plots for both the fixed effects, displaying influence on the coefficients and Cook's distances,
#     and the random-effect parameters
# ------------------------------------------------------------------------------

infl <- influence(mod_obj, groups = c("school"))

infl



# ----------

car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)

car::influenceIndexPlot(mod_obj, var = "var.cov.comps")

# help("influence.mixed.models")

