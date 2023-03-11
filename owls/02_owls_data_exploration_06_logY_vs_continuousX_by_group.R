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
# data exploration:  log Y vs. continuous X
# ------------------------------------------------------------------------------

head(Owls[Owls$Nest == "AutavauxTV", 1:5])



# -->
# The experiment was carried out on two nights, and the food treatment changed.
# Observations 1 and 2 were made at 22.25 and 22.38 hours, but the time difference between them is not 13 minutes, but 24 hours and 13 minutes !
# So, we have to be very careful where we place the auto-regressive correlation sturucture.
# It should be within a nest on a certain night.
# The random intercept and the compound correlation models place the correlation within the same nest, irrespective of the night.

# The observations are not regularly spaced, at least not from our point of view.
# However, from the owl parent's point of view, time between visits may be regularly spaced.
# With this we mean that it may well be possible that the parents chose the nest visiting times.
# Obviously, if there is not enough food, and the parents need a lot of effort or time to catch prey, this is not a valid assumption.
# But if there is a surplus of food, this may well be a valid assumption.
# Let us assume the owls indeed chose the times and therefore, we consider the longitudinal data as regularly spaced.


# This basically means that we assume that distances (along the time axis) betwen the vertical lines are all the same.
Owls$LogNeg <- log10(Owls$NegPerChick + 1)

xyplot(LogNeg ~ ArrivalTime | Nest, data = Owls, type = "h", col = 1, subset = (FoodTreatment == "Deprived"), main = "Deprived")

xyplot(LogNeg ~ ArrivalTime | Nest, data = Owls, type="h", col = 1, subset = (FoodTreatment == "Satiated"), main = "Satiated")

