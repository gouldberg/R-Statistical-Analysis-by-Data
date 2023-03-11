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
# Model interpretation by 3d wireframe plot
# ------------------------------------------------------------------------------
# 3-dimentional plot
NewData <- expand.grid(LatAbs = seq(from = 0.02, to = 65, length = 25),
                       CC     = seq(from = 1100, to = 1700, length = 25))

NewData$PredOrbitalVol <- predict(M2, NewData)
head(NewData)

pts <- data.frame(x = HVS2$LatAbs,
                  y = HVS2$CC,
                  z = HVS2$OrbitalV)

wireframe(PredOrbitalVol ~ LatAbs + CC, data = NewData,
          aspect = c(61/87, 0.4),
          light.source = c(0,0,10), 
          distance = .2,
          shade.colors.palette = function(irr, ref, height, w = .5)
            grey(w * irr + (1 - w) * (1 - (1-ref)^.4)),
          zlab = list(label = "Predicted orbital volume", rot = 90),
          shade = TRUE,
          scales = list(arrows = FALSE),
          pts = pts,
          panel.3d.wireframe =
            function(x, y, z,
                     xlim, ylim, zlim,
                     xlim.scaled, ylim.scaled, zlim.scaled,
                     pts,
                     ...) {
              xx <-
                xlim.scaled[1] + diff(xlim.scaled) *
                (pts$x - xlim[1]) / diff(xlim)
              yy <-
                ylim.scaled[1] + diff(ylim.scaled) *
                (pts$y - ylim[1]) / diff(ylim)
              zz <-
                zlim.scaled[1] + diff(zlim.scaled) *
                (pts$z - zlim[1]) / diff(zlim)
              
              panel.3dwire(x = x, y = y, z = z,
                           xlim = xlim,
                           ylim = ylim,
                           zlim = zlim,
                           xlim.scaled = xlim.scaled,
                           ylim.scaled = ylim.scaled,
                           zlim.scaled = zlim.scaled,
                           ...)
              
            })

