setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
levels(ICU2$cancer) <- c("-", "Cancer")
levels(ICU2$admit) <- c("-", "Emerg")
levels(ICU2$uncons) <- c("-", "Uncons")

icu.glm2 <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


# Main effect (default is response probabilities)

eff <- effects::allEffects(icu.glm2)


eff[["age"]]

eff[["age"]]$model.matrix %>% head()



# ----------
# plot main effets of each variable (default is reponse probabilities)
plot(eff)
# plot(predictorEffects(icu.glm2))

plot(eff, type = "response")



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(icu.glm2, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))

# eff2 <- effects::allEffects(icu.glm2, partial.residuals = TRUE)
# plot(eff2, rows = 2, cols = 2, type = "response", residuals.pch = 15)




# ----------
# separate representation
plot(Effect("age", icu.glm2))

plot(Effect("uncons", icu.glm2))

