setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
summary(qlmod)



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response probabilities)

eff <- effects::allEffects(qlmod)


eff[["log(body)"]]

eff[["log(body)"]]$model.matrix %>% head()



# ----------
# plot main effets of each variable (default is reponse ratio)
# plot(eff)
plot(predictorEffects(qlmod))

plot(eff, type = "response")



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(qlmod, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))
# eff2 <- effects::allEffects(qlmod, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)



# ----------
# separate representation
plot(Effect("body", qlmod))


