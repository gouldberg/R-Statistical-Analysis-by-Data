setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ----------
attenu <- na.exclude(attenu)


attenu <- attenu %>% mutate(mag2 = ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7)))



# ----------
mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu, REML = FALSE)


mod_obj <- mmod



# ------------------------------------------------------------------------------
# The collection of index plot
#   - for mixed-effects models, influence() refits the model deleting each of group in turn,
#     and provide deletion statistics
#   - influenceIndexPlot() create plots for both the fixed effects, displaying influence on the coefficients and Cook's distances,
#     and the random-effect parameters
# ------------------------------------------------------------------------------

infl <- influence(mod_obj, groups = c("station"))

infl



# ----------

car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)

car::influenceIndexPlot(mod_obj, var = "var.cov.comps")

# help("influence.mixed.models")

