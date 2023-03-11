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



# ----------
library(lme4)

mmod <- lmer(math ~ raven * social * gender + (1 | school) + (1 | school : class), data = jspr)

jspr$craven <- jspr$raven - mean(jspr$raven)
mmod_final <- lmer(math ~ craven * social + (1 | school) + (1 | school : class), data = jspr)



# ----------
mod_obj <- mmod_final



# ------------------------------------------------------------------------------
# Model understanding:  Fixed Effects including interactions with craven and social
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(mod_obj))




# ----------
# only craven
plot(predictorEffects(mod_obj, ~craven))

