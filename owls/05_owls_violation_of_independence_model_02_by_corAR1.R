setwd("//media//kswada//MyFiles//R//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------
Owls <- read.table(file = "//media//kswada//MyFiles//references//ZuurDataMixedModelling//Owls.txt", header = TRUE)

str(Owls)

Owls$LogNeg <- log10(Owls$NegPerChick + 1)




# ------------------------------------------------------------------------------
# Auto-regressive correlation structure of order 1  --> cor(eis, eit) = Rho^|t - s|
#   - Based on the biological knowledge of these owls, it is more natural to assume that observations made close to each other
#     in time are more similar than those separated further in time.
# ------------------------------------------------------------------------------

head(Owls[Owls$Nest == "AutavauxTV", 1:5])



# -->
# The experiment was carried out on two nights, and the food treatment changed.
# Observations 1 and 2 were made at 22.25 and 22.38 hours, but the time difference between them is not 13 minutes, but 24 hours and 13 minutes !
# So, we have to be very careful where we place the auto-regressive correlation sturucture.
# It should be within a nest on a certain night.
# The random intercept and the compound correlation models place the correlation within the same nest, irrespective of the night.

# THe observations are not regularly spaced, at least not from our point of view.
# However, from the owl parent's point of view, time between visits may be regularly spaced.
# With this we mean that it may well be possible that the parents chose the nest visiting times.
# Obviously, if there is not enough food, and the parents need a lot of effort or time to catch prey, this is not a valid assumption.
# But if there is a surplus of food, this may well be a valid assumption.
# Let us assume the owls indeed chose the times and therefore, we consider the longitudinal data as regularly spaced.


# This basically means that we assume that distances (along the time axis) betwen the vertical lines are all the same.
xyplot(LogNeg ~ ArrivalTime | Nest, data = Owls, type = "h", col = 1, subset = (FoodTreatment == "Deprived"), main = "Deprived")

xyplot(LogNeg ~ ArrivalTime | Nest, data = Owls, type="h", col = 1, subset = (FoodTreatment == "Satiated"), main = "Satiated")



# ----------
M1.lme = lme(Form, random = ~ 1 | Nest, method = "REML", data = Owls)

M2.gls <- gls(Form, correlation = corCompSymm(form = ~ 1 | Nest), method = "REML", data = Owls)



# The variables FoodTreatment and Nest identify the group of observations from the same night, and the correlation is applied within this group.
# As a result, the index i in the model does not represent nest, but night in the nest.
M3.gls <- gls(Form, correlation = corAR1(form = ~ 1 | Nest / FoodTreatment), method = "REML", data = Owls)

M4 <- lme(Form, random = ~ 1 | Nest, correlation = corAR1(form = ~ 1 | Nest / FoodTreatment), method = "REML", data = Owls)

summary(M2.gls)

summary(M3.gls)



# -->
# Estimated auto-correlation Phi = 0.418, which is relatively high.


# The model with the auto-regressive correlation structure assumes that observations from different nests are independent and also that the
# observations from the same nest on two different nights are independent.

# It may be an option to extend the model with the AR1 correlation structure with a random intercept nest.
# Such a model allows for the compound correlation between all observations from the same nest, and temporal correlation between
# observations from the same nest AND night.

# But the danger is that the random intercept and auto-correlation will fight with each other for the same information.



