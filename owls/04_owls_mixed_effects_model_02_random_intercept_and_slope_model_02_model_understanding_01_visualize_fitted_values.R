# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ----------
Form <- formula(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime)

M1.lme <- lme(Form, random = ~ 1 | Nest, method = "REML", data = Owls)

M2.lme <- lme(Form, random = ~ 1 + ArrivalTime | Nest, method = "REML", data = Owls)

mod_obj <- M2.lme
mod_obj2 <- M1.lme



# ------------------------------------------------------------------------------
# fitted value of random intercept and slope model
# ------------------------------------------------------------------------------

# level = 0:  we take the fitted values obtained by the population model
# level = 1: give the within-Nest fitted values.


# show first 100 samples
# Compare random intercept model and random intercept and slope model
# Note that value of level = 0 is same for both models 
graphics.off()
par(mfrow = c(1,1))

F0 <- fitted(mod_obj, level = 0)
act <- Owls$LogNeg


plot(act[1:100], col = "black", cex = 0.8, type = "h", ylim = c(0, 0.9), xlab = "", ylab = "")
par(new = T)
plot(F0[1:100], col = "black", pch = 2, cex = 0.8, ylim = c(0, 0.9), xlab = "", ylab = "")


F1 <- fitted(mod_obj, level = 1)
par(new = T)
plot(F1[1:100], col = "blue", pch = 20, cex = 0.8, ylim = c(0, 0.9), xlab = "", ylab = "")


F1 <- fitted(mod_obj2, level = 1)
par(new = T)
plot(F1[1:100], col = "red", pch = 20, cex = 0.8, ylim = c(0, 0.9), xlab = "", ylab = "")



# -->
# type = "h":  actual value
# black triangle:  population model
# blue:  random intercept model
# red:  random intercept and slope model



# ------------------------------------------------------------------------------
# Effect plot
# ------------------------------------------------------------------------------

library(effects)

# but this shows only population model
plot(predictorEffects(mod_obj))


