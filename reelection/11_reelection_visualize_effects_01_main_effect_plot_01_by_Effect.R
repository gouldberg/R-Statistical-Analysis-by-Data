setwd("//media//kswada//MyFiles//R//reelection")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Reelection
# ------------------------------------------------------------------------------

data("Reelection", package = "pder")


str(Reelection)


dim(Reelection)


car::some(Reelection)



# ----------
elect.l <- glm(reelect ~ ddefterm + ddefey + gdppc + dev + nd + maj,
               data = Reelection,
               family = "binomial", subset = narrow)


elect.p <- update(elect.l, family = binomial(link = "probit"))



mod_obj <- elect.l

# mod_obj <- elect.p



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response probabilities)

eff <- effects::allEffects(mod_obj)


# ----------
# plot main effets of each variable (default is reponse probabilities)
plot(eff)
plot(predictorEffects(mod_obj))


plot(eff, type = "response")




# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))

# eff2 <- effects::allEffects(mod_obj, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)




# ----------
# separate representation
plot(Effect("age", icu.glm2))

plot(Effect("uncons", icu.glm2))

