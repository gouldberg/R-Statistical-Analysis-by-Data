setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------
data("psid", package = "faraway")

str(psid)

car::some(psid)



# ----------
psid$cyear <- psid$year - 78

mod_obj <- lmer(log(income) ~ cyear * sex + age + educ + (cyear | person), data = psid)



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("cyear", "educ"), mod_obj), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(5,4,3,2,1), col = c(gray(0.8), gray(0.6), gray(0.4), gray(0.2), "black")))


