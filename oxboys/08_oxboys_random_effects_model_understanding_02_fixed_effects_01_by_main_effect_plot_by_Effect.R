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
library(nlme)

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)


m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = d,
          random = list(Subject = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


mod_obj <- m0



# ------------------------------------------------------------------------------
# Model understanding:  Fixed Effects
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(mod_obj))



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))

# eff2 <- effects::allEffects(mod_obj, partial.residuals = TRUE)


