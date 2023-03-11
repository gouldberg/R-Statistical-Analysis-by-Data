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
# The collection of index plot
#   - for mixed-effects models, influence() refits the model deleting each of group in turn,
#     and provide deletion statistics
#   - influenceIndexPlot() create plots for both the fixed effects, displaying influence on the coefficients and Cook's distances,
#     and the random-effect parameters
# ------------------------------------------------------------------------------

infl <- influence(mod_obj, groups = c("person"))

infl



# ----------

car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)

car::influenceIndexPlot(mod_obj, vars = "var.cov.comps")

# help("influence.mixed.models")

