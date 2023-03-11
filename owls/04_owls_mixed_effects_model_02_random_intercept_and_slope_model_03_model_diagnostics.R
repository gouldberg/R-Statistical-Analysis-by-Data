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




# ------------------------------------------------------------------------------
# Model validation
# ------------------------------------------------------------------------------

E2 <- resid(mod_obj, type = "normalized")

F2 <- fitted(mod_obj)

op <- par(mfrow=c(2,2), mar=c(4,4,3,2))

MyYlab = "Residuals"

plot(x = F2, y = E2, xlab = "Fitted values", ylab = MyYlab)

boxplot(E2 ~ SexParent, data = Owls, main="Sex of parent", ylab = MyYlab)

boxplot(E2 ~ FoodTreatment, data = Owls, main="Food treatment", ylab = MyYlab)

plot(x = Owls$ArrivalTime, y = E, main = "Arrival time", ylab = MyYlab, xlab = "Time (hours)")

par(op)



# -->
# These graphs do not show any clear violation of heterogeneity except residuals versus arrival time, which is not clear.
# For the moment, we ignore any potential independence problems.

