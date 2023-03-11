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
# Poisson GLM and model selection
# ------------------------------------------------------------------------------

library(nlme)

Owls$NCalls <- Owls$SiblingNegotiation

Owls$LBroodSize <- log(Owls$BroodSize)



# ----------
Form <- formula(NCalls ~ offset(LBroodSize) + SexParent * FoodTreatment + SexParent * ArrivalTime)

O1 <- glm(Form, family = quasipoisson, data = Owls)



summary(O1)



# -->
# There is overdispersion



# ----------
drop1(O1, test = "F")



# -->
# Indicate that the two 2-way interactions are not significant.


O2 <- update(O1, . ~ . - SexParent * ArrivalTime)


drop1(O2, test="F")



# ----------
# Using a backwards selection, we ended up with the model containing food treatment and arrival time.
# There was overdispersion, therefore, we refitted the model with a quasi-Poisson GLM.
Form <- formula(NCalls ~ offset(LBroodSize) + FoodTreatment + ArrivalTime)

O3 <- glm(Form, family = quasipoisson, data = Owls)

drop1(O3, test="F")


# -->
# All regression parameters are highly significant.

