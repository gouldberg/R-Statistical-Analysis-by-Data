# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------

load("./data/mite.RData")


dim(mite)
car::some(mite)


dim(mite.env)
car::some(mite.env)


dim(mite.xy)
car::some(mite.xy)




# ------------------------------------------------------------------------------
# Rarefaction analysis
#   - generally, one computes the number of expected species (one "rarefies") to a "sample size"
#     (remember that here this refers to the number of individuals) equal to the number of individuals of the sampling unit
#     where the abundance is the smallest.
# ------------------------------------------------------------------------------

# Number of species in the 70 moss or soil cores
( mite.nbsp <- specnumber(mite) )



# ----------
# Cores with minimum and maximum observed species richness
# The actual number of species in the data set range from 5 to 25.

range(mite.nbsp)


mite.nbsp[mite.nbsp == min(mite.nbsp)]

mite.nbsp[mite.nbsp == max(mite.nbsp)]



# ----------
# Total abundance in the 70 cores: huge difference
( mite.abund <- rowSums(mite) )

range(mite.abund)




# ----------
# Abundance in the cores with smallest and largest number of species

# One of the 2 cores with the smallest observed number of individuals (core #57) is also the one with the smallest number of species

mite.abund[mite.nbsp == min(mite.nbsp)]

mite.abund[mite.nbsp == max(mite.nbsp)]



# ----------
# Number of species in the core with smallest and largest abundance

# The core with the largest abundance (core #67) also contains very few species (only 6)

mite.nbsp[mite.abund == min(mite.abund)]

mite.nbsp[mite.abund == max(mite.abund)]
