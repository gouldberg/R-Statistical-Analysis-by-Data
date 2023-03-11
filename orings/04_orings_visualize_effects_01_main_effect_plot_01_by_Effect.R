setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response probabilities)

eff <- effects::allEffects(lmod)


eff[["temp"]]

eff[["temp"]]$model.matrix %>% head()



# ----------
# plot main effets of each variable (default is reponse ratio)
# plot(eff)
plot(predictorEffects(lmod))

plot(eff, type = "response")



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(lmod, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))
# eff2 <- effects::allEffects(lmod, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)



# ----------
# separate representation
plot(Effect("temp", lmod))


