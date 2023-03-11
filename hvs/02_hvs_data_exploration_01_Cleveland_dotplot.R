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
# Obaservations considerable smaller or larger than the majority:  multi-panel Cleveland dotplots
# ------------------------------------------------------------------------------

Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)),
               groups=FALSE,
               strip = strip.custom(bg = 'white',
                                    par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE),
                             y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data", cex = 1.5))
  
  print(P)  
}


MyVar <- c("OrbitalV", "LatAbs", "CC", "FM", "Illuminance", "Temperature")

Mydotplot(HVS2[, MyVar])


# -->
# These graphs indicate that there are no observations of notable smaller or larger values.
# hence there are no obvious outliers in the data.

