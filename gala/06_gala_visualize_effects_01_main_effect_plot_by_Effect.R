setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
mod_obj <- modp.step2
# mod_obj <- modp2



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response, not linked)
eff <- effects::allEffects(mod_obj)


eff[["log(Area)"]]

eff[["log(Area)"]]$model.matrix %>% head()



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
# eff2 <- effects::allEffects(qlmod, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)


# -->
# we notice that Nearest, and Scruz effect is very small.



# ----------
# separate representation
plot(Effect("Area", mod_obj))



