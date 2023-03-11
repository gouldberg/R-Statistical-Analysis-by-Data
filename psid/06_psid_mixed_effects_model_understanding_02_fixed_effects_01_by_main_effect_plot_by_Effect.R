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
# Model understanding:  Fixed Effects
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(mod_obj))




# ----------
# only cyear
plot(predictorEffects(mod_obj, ~cyear))

