setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ----------
lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)

m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = d,
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


mod_obj <- m0



# ------------------------------------------------------------------------------
# The collection of index plot
#   - for mixed-effects models, influence() refits the model deleting each of group in turn,
#     and provide deletion statistics
#   - influenceIndexPlot() create plots for both the fixed effects, displaying influence on the coefficients and Cook's distances,
#     and the random-effect parameters
# ------------------------------------------------------------------------------

# IT TAKES TIME !!! -- 
infl <- influence(mod_obj)

infl



# ----------
# influenceIndexPlot is not available for lme object

# car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)

# car::influenceIndexPlot(mod_obj, var = "var.cov.comps")

# help("influence.mixed.models")

