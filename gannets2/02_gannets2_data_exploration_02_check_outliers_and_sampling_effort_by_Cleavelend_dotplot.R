setwd("//media//kswada//MyFiles//R//gannets2")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gannets2
# ------------------------------------------------------------------------------

Gannets <- read.table(file = "Gannets2.txt", header = TRUE)


str(Gannets)
names(Gannets)



# ------------------------------------------------------------------------------
# Data exploration:  Check sampling effort differences per transect by Cleveland dotplot
#  - It double effort does not imply doble the number of birds, we can consider using sampling effort as a covariate in the model
# ------------------------------------------------------------------------------

plot(x = Gannets$Area_surveyedkm2, y = 1:nrow(Gannets),
     pch = 16, cex = 0.8, xlab = "Area", ylab = "Order of the data")



# -->
# Note that there are substantial size difference.
# It is crucial to take this into account in the analyses.
# For example, if we have a transect with a length of 100 m and another of 500 m, there is a high chance of recording greater abundance in the second transect.

# The problem arises in that there are a few transects of size 0 (presumably the boat did not move)
# We will remove these observations.



# ----------
# We remove observations with Area_surveyedkm2 == 0
Gannets2 <- Gannets[Gannets$Area_surveyedkm2 > 0,]


# The natural logarithm of Area_surveyedkm2 will be used as offset variable in the GAM
Gannets2$LArea <- log(Gannets2$Area_surveyedkm2)


# TimeH is time inhours, on a continuous scale where 14.50 is 14.30 h
Gannets2$TimeH <- Gannets2$Hours + Gannets2$Minutes / 60


# concatenate the time variables and calculates the Julian day within the year using the strptime function
Gannets2$Date <- paste(Gannets2$Day,Gannets2$Month,Gannets2$Year, sep = "/")
Gannets2$DayInYear <- strptime(Gannets2$Date, "%d/%m/%Y")$yday + 1


# calculate the number of days between sampling day and 1 January 1991.
# This variable can be used to make a time series graph of the number of gannets.
Gannets2$DaySince0 <- ceiling(julian(strptime(Gannets2$Date, format = "%d/%m/%Y"), origin = as.Date("1991-01-01")))

