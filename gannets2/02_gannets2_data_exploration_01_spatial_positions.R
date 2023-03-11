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
# Data exploration:  Spatial positions
# ------------------------------------------------------------------------------
# In this example, the "transect" is a series of observations covering a few hundred metres, and they are made over a short time span.
# The key point is that the total number of observations per unit surface area are recorded.
# hence each transect is represented by one value in the spreadsheet.

Gannets$Xkm <- Gannets$X / 1000
Gannets$Ykm <- Gannets$Y / 1000



# aspect = "iso" is crucial, as it ensures that units along the x-axis are the same as those along the y-axis.
xyplot(Ykm ~ Xkm | factor(Year), aspect = "iso",
       strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)), data = Gannets, pch=16, 
       cex = 0.4, col = 1, xlab = list(label = "Xkm", cex = 1.5), ylab = list(label = "Ykm", cex = 1.5))


# -->
# This shows the locations of the transects per sampling year.
# Each transect is represented by a point, but, because of the large number of observations (i.e. transects) made during a boat survey,
# it appears as though we have a continuous line, although that is not the case.
# It seems that in each year roughly the same areas were sampled.

