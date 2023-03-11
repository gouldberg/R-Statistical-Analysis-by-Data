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



# ------------------------------------------------------------------------------
# Fit the model with GLS
#   - IT allows us to compare the linear regression model with the mixed effects model that we will calculate using the lme funciton
# ------------------------------------------------------------------------------

Owls$LogNeg <- log10(Owls$NegPerChick + 1)


library(nlme)

Form <- formula(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime)

M.gls <- gls(Form, data=Owls)

summary(M.gls)



# -->
# The numerical output in the object M.gls is identical to that of the lm function.
