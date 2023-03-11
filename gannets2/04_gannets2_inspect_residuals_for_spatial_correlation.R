setwd("//media//kswada//MyFiles//R//gannets2")

packages <- c("dplyr", "lattice", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gannets2
# ------------------------------------------------------------------------------

Gannets <- read.table(file = "Gannets2.txt", header = TRUE)


str(Gannets)
names(Gannets)


Gannets$Xkm <- Gannets$X / 1000
Gannets$Ykm <- Gannets$Y / 1000
Gannets2 <- Gannets[Gannets$Area_surveyedkm2 > 0,]
Gannets2$LArea <- log(Gannets2$Area_surveyedkm2)
Gannets2$TimeH <- Gannets2$Hours + Gannets2$Minutes/60
Gannets2$Date <- paste(Gannets2$Day,Gannets2$Month,Gannets2$Year, sep = "/")
Gannets2$DayInYear <- strptime(Gannets2$Date, "%d/%m/%Y")$yday + 1
Gannets2$DaySince0 <- ceiling(julian(strptime(Gannets2$Date, format = "%d/%m/%Y"), origin = as.Date("1991-01-01")))
Gannets2$G     <- Gannets2$Gannets_in_transect
Gannets2$GProp <- Gannets2$G / Gannets2$Area



# ------------------------------------------------------------------------------
# Inspect the residuals for spatial correlation
# ------------------------------------------------------------------------------
library(sp)

library(gstat)


mydata <- data.frame(E8, Gannets2$Xkm, Gannets2$Ykm)

coordinates(mydata) <- c("Gannets2.Xkm", "Gannets2.Ykm")


V12 <- variogram(E8 ~ 1, mydata, cutoff = 5, robust = TRUE)

plot(x = V12$dist, y = V12$gamma, xlab = "Distance (km)", ylab = "Semi-variogram", pch = 16, cex = 2 *V12$np / max(V12$np))



# -->
# Ideally the sample variogram should resemble a horizontal band of points, as this would indicate spatial independence.
# Spatial dependence is present if the variogram values increase at greater distances between sites.
# The pattern does not indicate any serious spatial correlation between sites that are separated by 5km or less.

