# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ----------
mod_obj <- mod.nbin



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response, not linked)
eff <- effects::allEffects(mod_obj)



# ----------
# plot main effets of each variable (default is reponse)
# plot(eff)
plot(predictorEffects(mod_obj))

plot(eff, type = "response")




# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))
# eff2 <- effects::allEffects(qlmod, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)



