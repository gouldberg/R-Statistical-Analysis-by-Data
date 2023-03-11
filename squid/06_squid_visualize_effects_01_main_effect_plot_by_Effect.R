# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------
Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


Squid$fMONTH = factor(Squid$MONTH)



# ----------
vf4 <- varPower(form = ~ DML | fMONTH)
M.gls4 <- gls(Testisweight ~ DML * fMONTH, data = Squid, weights = vf4)
summary(M.gls4)

mod_obj <- M.gls4



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response, not linked)
eff <- effects::allEffects(mod_obj)


eff



# ----------
# plot main effets of each variable (default is reponse)
# For a Poisson GLM, an important feature is that the response is plotted on the log scale, so that effects in the model appear as linear functions.
# while the values of the response (number of articles) are labeled on their original scale, facilitating interpretation.
# plot(eff)
plot(predictorEffects(mod_obj))

plot(eff, type = "response")




# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))
# eff2 <- effects::allEffects(mod_obj, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)




