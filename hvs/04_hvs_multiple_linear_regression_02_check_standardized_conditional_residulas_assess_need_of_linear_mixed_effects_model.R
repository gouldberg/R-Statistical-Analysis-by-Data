setwd("//media//kswada//MyFiles//R//hvs")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HVS
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



# ------------------------------------------------------------------------------
# Check residuals conditional on population
# ------------------------------------------------------------------------------

# Model residuals conditional on population
T1 <- lm(E2 ~ fPopulation, data = HVS2)



# Single term deletion
drop1(T1, test = "F")



# -->
# F-test indicates that there is no significant population effect in the residulas



# ----------
boxplot(E2 ~ fPopulation, 
        data = HVS2, 
        cex.lab = 1.5,
        xlab = "Population",
        ylab = "Standardized residuals")

abline(0, 0, lty = 2)




# ------------------------------------------------------------------------------
# Should we have applied a mixed effects model ?
# ------------------------------------------------------------------------------

# Number of observations per population in case we use mixed effect models
table(HVS2$fPopulation)


boxplot(OrbitalV ~ fPopulation, 
        data = HVS2, 
        cex.lab = 1.5,
        xlab = "Population",
        ylab = "Orvital Vulume")

