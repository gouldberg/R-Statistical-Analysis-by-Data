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
# Model understanding:  Person effect
# ------------------------------------------------------------------------------

# These represent person effect adjusted for year
ref <- ranef(mod_obj)$person[[1]]

summary(ref)



# -->
# The difference between the best and the worst is about 2.8.



densityPlot(ref)