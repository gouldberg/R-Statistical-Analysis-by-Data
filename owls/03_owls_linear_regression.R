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
# Linear Regression
# ------------------------------------------------------------------------------

M.lm <- lm(NegPerChick ~ SexParent * FoodTreatment + SexParent * ArrivalTime, data = Owls)


summary(M.lm)


