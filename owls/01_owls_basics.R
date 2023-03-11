# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
#   - Roulin and Bersier (2007) analysed the begging behaviour of nestling barn owls.
#     They looked at how nestlings responded to the presence of the father and of the mother. Using microphones inside and a video outside the nests,
#     they sampled 27 nests and studied vocal begging behaviour when the parents brought prey.
#     The number of nestlings was between 2 and 7 per nest.
#   - Different response variables were defined in the paper:
#       - the amount of time spent on the perch by a parent,  the amount of time in the nestbox,  sibling negotiation, and begging
#   - We analyse sibing negotiation, which is defined as follows.
#     Using the recorded footage, the number of calls made by all offspring in the absence of the parents was counted during 30-s time intervals every 15 min.
#     To allocate a number of calls to a visit from a parent, the counted number of calls from the preceding 15 min of the arrival was used.
#     This number was then divided by the number of nestlings.
#   - The explanatory variables are sex of the parent, treatment of food, and arrival time of the parent.
#     Half of the nests were given extra prey, and from the other half, prey (remaining) were removed. These were called 'food-satiated' and 'food-deprived'
#     respectively.
#     Measurements took place on two nights, and the food treatment was swapped on the second night. Note that the original paper countains an ethical note
#     stating that food treatment did not have an effect on survival of the chicks. Measurements took place between 21.30 h and 05.30 h and the variable
#     ArrivalTime reflects the time when a parent arrived at the perch with a prey.
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ------------------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------------------

