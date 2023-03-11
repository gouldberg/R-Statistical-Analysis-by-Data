setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ----------
mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)


mod_obj <- mmod



# ------------------------------------------------------------------------------
# The collection of index plot
#   - for mixed-effects models, influence() refits the model deleting each of group in turn,
#     and provide deletion statistics
#   - influenceIndexPlot() create plots for both the fixed effects, displaying influence on the coefficients and Cook's distances,
#     and the random-effect parameters
# ------------------------------------------------------------------------------

infl <- influence(mod_obj, groups = "operator")

infl



# ----------

car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat", "dfbetas"), id.n = 4)

# car::influenceIndexPlot(mod_obj, vars = "var.cov.comps")

# help("influence.mixed.models")

