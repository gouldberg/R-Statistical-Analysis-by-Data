setwd("//media//kswada//MyFiles//R//hvs")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HVS
#   - Ambient light levels influence the size of the components of the visual system in birds and primates, and Pearce and Funbar (2011) argue that
#     the same is true for humans.
#     Using linear regression techniques, they demonstrated a significant positive relationship between absolute geographic latitude
#     and human orbital volume, an index of eyeball size.  Owning to tight scaling among visual system components,
#     differences in eyeball size will translate to size differences in visual cortices.
#     They concluded that light levels, as reflected by absolute latitide, drive variation in visual system size in the human population.
#   - Pearce and Dunbar (2011) measured cranial capacity (CC), orbital volumne and foramen magnum (FM) dimensions in 55 adult crania from
#     the Oxford University Museum of Natural History and the Duckworth Collection, University of Cambridge.
#     The response variable is mean orbital volume.  The value for ortbital volume represents the mean of 3 replicate measurements of each cranium.
#   - Variables:
#        - Mean Orbital Volume:  Index of eyeball size
#        - Cranial Capacity:  Volume of the interior of the cranium
#        - Minimum Illuminance:  Light intensity (on the log scale)
#        - Minimum Temperature:  Minimum temperature
#        - FMarea intercondyle (foramen magnum):  Proxy for body mass, index for the volume of neural tissue required for somantic maintenance
#        - Absolute Latitude:  Absolute latitude of sample site
#        - Gender:  Male / Female
#        - Population:  Geographical region
# ------------------------------------------------------------------------------
HVS <- read.table(file = "HVS.txt", header = TRUE, dec=".")


str(HVS)

names(HVS)


# ----------
# The variable names are long, we will rename some of them
# We also define categorical variables as factors, to avoid mistakes.
HVS$OrbitalV    <- HVS$MeanOrbitalVolume
HVS$fPopulation <- factor(HVS$Population)
HVS$LatAbs      <- HVS$AbsoluteLatitude
HVS$CC          <- HVS$CranialCapacity
HVS$FM          <- HVS$FMarea_intercondyle
HVS$Illuminance <- HVS$Minimum_Illuminance
HVS$Temperature <- HVS$Minimum_Temperature_celsius
HVS$fGender     <- factor(HVS$Gender)



# ------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------

# There is one missing value
colSums(is.na(HVS))


# we remove it
# The function na.exvlude removes all rows that contain a missing value.
HVS2 <- na.exclude(HVS)


