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
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("raven", "social"), mod_obj), 
     confint = list(style = "bars"),
     lines = list(multiline = TRUE, lty = c(5,4,3,2,1), col = c(gray(0.8), gray(0.6), gray(0.4), gray(0.2), "black")))


