setwd("//media//kswada//MyFiles//R//spasthma")

packages <- c("dplyr", "rgdal", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  spasthma
# ------------------------------------------------------------------------------
library(rgdal)

spasthma <- readOGR("//media//kswada//MyFiles//data//spasthma", "spasthma")
spbdry <- readOGR("//media//kswada//MyFiles//data//spasthma//spbdry.shp", "spbdry")
spsrc <- readOGR("//media//kswada//MyFiles//data//spasthma//spsrc.shp", "spsrc")
sproads <- readOGR("//media//kswada//MyFiles//data//spasthma//sproads.shp", "sproads")


# ----------
# SpatialPointsDataFrame
str(spasthma)

# SpatialPolygonsDataFrame
str(spbdry)

# SpatialPointsDataFrame
str(spsrc)

# SpatialLinesDataFrame
str(sproads)



# ------------------------------------------------------------------------------
# preparation
# ------------------------------------------------------------------------------
pppasthma <- as(spasthma, "ppp")
pppasthma$window <- as(spbdry, "owin")

marks(pppasthma) <- relevel(pppasthma$marks$Asthma, "control")

pppasthma
str(pppasthma)



# ----------
cases <- unmark(subset(pppasthma, marks(pppasthma) =="case"))
controls <- unmark(subset(pppasthma, marks(pppasthma) =="control"))

ncases <- npoints(cases)
ncontrols <- npoints(controls)



# ------------------------------------------------------------------------------
# estimate bandwidth
# ------------------------------------------------------------------------------
# Kelsall and Diggle (1998) suggest cross-validation criterion.
( rrbw <- bw.relrisk(pppasthma, hmax=.5) )

bwasthmap <- 0.06 


# -->
# Under this criterion, we obtained a bandwidth of 0.209.
# However, we believe that this value would over-smooth the data and we have set it to 0.06, as in the estimation of the relative risk ratio



# ------------------------------------------------------------------------------
# estimate the probability of being a case at given location
# ------------------------------------------------------------------------------
rr <- relrisk(pppasthma, bwasthmap)


# ----------
spkratio$prob <- as(as(rr, "SpatialGridDataFrame"), "SpatialPixelsDataFrame")$v
ats <- seq(0,max(spkratio$prob), length.out=11)
cols <- colorRampPalette(brewer.pal(8, "Reds"))(length(ats)-1)
print(spplot(spkratio, "prob", col.regions=cols, at=ats, sp.layout=list(lytb, lytp)))


