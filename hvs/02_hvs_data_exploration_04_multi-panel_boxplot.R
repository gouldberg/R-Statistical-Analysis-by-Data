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
# Assess collineartity between continuous covariates and categorical covariates:  multi-panel boxplots
# ------------------------------------------------------------------------------

Mybwplot <- function(Z, MyVar, TargetVar){
  #Multipanel boxplots
  #Z: data set
  #MyVar: character string
  #TargetVar: variable for the x-axis..must be a factor
  
  AllY <- as.vector(as.matrix(Z[,MyVar]))
  AllX <- rep(Z[,TargetVar], length(MyVar))
  ID <- rep(MyVar, each = nrow(Z))
  
  P <- bwplot(AllY ~ factor(AllX) | ID, horizontal = FALSE,
              ylab = "", xlab = "",
              scales = list(alternating = T,cex.lab = 1.5,
                            x = list(relation = "same",rot =90, abbreviate = TRUE, cex = 1.5),
                            y = list(relation = "free", draw = FALSE)),
              strip = strip.custom(bg = 'white',
                                   par.strip.text = list(cex = 1.2)),
              cex = .5,
              par.settings = list(
                box.rectangle = list(col = 1),
                box.umbrella  = list(col = 1),
                plot.symbol   = list(cex = .5, col = 1)))
  print(P)
}


MyVar2 <- c("LatAbs", "CC", "FM", "Illuminance", "Temperature")

Mybwplot(HVS2, MyVar2, "fGender")


# -->
# Results indicate that cranial capacity values are slightly higher in males, indicating potential collinearity between CC and gender.

