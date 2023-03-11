setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

# linmod <- lm(involact ~ race + fire + theft + age + income, data = chredlin)



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


eff <- effects::allEffects(linmod)


eff



# ----------
# plot main effets of each variable

plot(eff)

plot(predictorEffects(linmod))

predictorEffects(linmod)



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve

plot(predictorEffects(linmod, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))


