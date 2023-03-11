setwd("//media//kswada//MyFiles//R//spasthma")

packages <- c("dplyr", "rgdal")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  spasthma
#   - The Asthma data set records the results of a case-control study carried out in 1992 on the incidence of asthma in children in North Derbyshire
#     (United Kingdom).  This data set has been studied by Diggle and Rowlingson (1994), Singleton et al. (1995), and Diggle (2003) to explorer
#     the relationship between asthma and the proximity to the main roads and three putative pollution sources (a coking works, chemical plant, and waste treatment centre).
#   - In the study, a number of relevant covariates were also collected by means of a questionnaire that was completed by the parents of the children attending
#     ten schools in the region.
#     Children having suffered from asthma will act as cases whilst the remainder of the children included in the study will form the set of controls.
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
# Display the point pattern, including the boundary of the region and the location of the pollution sources
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(spbdry, axes=TRUE, lwd=0.5)
plot(sproads, add=TRUE, lwd=2, col="darkslategrey")

c_c <- (spasthma$Asthma == "case") + 1
plot(spasthma[c_c == 1,], add=TRUE, pch=4, cex=0.6, col="mediumaquamarine")
plot(spasthma[c_c == 2,], add=TRUE, pch=17, cex=0.75, col="goldenrod2")
plot(spsrc, pch=22, add=TRUE, cex=1.2, bg="brown4")
legend("bottomright", legend=c("controls", "cases", "pollution sources"), pch=c(4, 17, 22), pt.cex=c(0.6, 0.75, 1.2), pt.bg=c(NA, NA, "brown4"), col=c("mediumaquamarine", "goldenrod2", "black"), bty="n") 

