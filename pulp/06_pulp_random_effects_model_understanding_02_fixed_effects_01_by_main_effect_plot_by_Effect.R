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
lmod <- aov(bright ~ operator, data = pulp)

mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)

mod_obj <- lmod



# ------------------------------------------------------------------------------
# Model understanding:  Fixed Effects
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(mod_obj))


# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))

# eff2 <- effects::allEffects(mod_obj, partial.residuals = TRUE)



# ------------------------------------------------------------------------------
# Model understanding:  Fixed Effects by model.tables
#   - model.tables:  compute tables of results from an aov model fit
# ------------------------------------------------------------------------------

# This is actually same as below
model.tables(mod_obj, type = "means")

model.tables(mod_obj, type = "effects", se = TRUE)


