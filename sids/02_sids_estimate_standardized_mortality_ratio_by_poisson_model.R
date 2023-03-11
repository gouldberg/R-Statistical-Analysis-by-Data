setwd("//media//kswada//MyFiles//R//sids")

packages <- c("dplyr", "maptools", "spdep")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sids
# ------------------------------------------------------------------------------
# read the SIDS data, boundaries of North Carolina and the adjacency structure of the North Carolina countries in GAL format.
nc_file <- system.file("shapes/sids.shp", package="spData")[1]

llCRS <- CRS("+proj=longlat +datum=NAD27")
nc <- readShapePoly(nc_file, ID="FIPSNO", proj4string=llCRS)

str(nc)


# ----------
# By using the argument region.id we made sure that the order of the list of neighbours ncCR85 is the same as the areas in the
# SpatialPolygonDataFrame object nc
( rn <- sapply(slot(nc, "polygons"), function(x) slot(x, "ID")) )

gal_file <- system.file("weights/ncCR85.gal", package="spData")[1]
ncCR85 <- read.gal(gal_file, region.id = rn)

ncCR85




# ------------------------------------------------------------------------------
# Calculate standardized mortality ratio (SMR) = Observed / Expected
# ------------------------------------------------------------------------------
# the population at risk is the number of births
nc$Observed <- nc$SID74

nc$Population <- nc$BIR74

r <- sum(nc$Observed)/sum(nc$Population)

( nc$Expected <- nc$Population * r )

( nc$SMR <- nc$Observed/nc$Expected )



# ----------
library(RColorBrewer)

# Used method proposed by Nicky Best
logSMR <- log(nc$SMR[nc$SMR > 0])
nsteps <- 7
step <- (max(logSMR) - min(logSMR)) / nsteps
brks <- exp(min(logSMR) + (0:nsteps) * step)
brks[1] <- 0
cols <- c(rev(brewer.pal(3, "Blues")), brewer.pal(4, "Reds"))
grps <- as.ordered(cut(nc$SMR, brks, include.lowest=TRUE))


plot(nc, col=cols[unclass(grps)], axes = FALSE)
box()
degAxis(1)
degAxis(2, at=c(34,35,36,37)) 
legend("bottomleft",legend=levels(grps), fill=cols, bty="n",cex=0.8,y.intersp=0.8) 



# ------------------------------------------------------------------------------
# confidence interval for each SMR
# ------------------------------------------------------------------------------
# based on the fact that Observed(i) is Poisson distributed, we can obtain a confidence interval for each SMR

library(epitools)
CISMR <- pois.exact(nc$Observed, nc$Expected)


plot(1,1, type="n", xlim=c(1,100), ylim=c(0,9), main= "Confidence intervals of the SMR", xlab="County", ylab="Relative Risk", xaxt="n")
abline(h=1, lty=2)

for(i in 1:100) {
  if(CISMR$lower[i]>1 ) {
    #sig.col <- 'red'
    sig.col <- brewer.pal(4, "Reds")[4]
    col <- sig.col
    lty <- 2
    text(i, CISMR$upper[i]+.31, nc$NAME[i],
         srt=90, col=sig.col, cex=.85)
  } else {
    col <- "black"
    lty <- 1
  }
  lines(c(i,i), c(CISMR$lower[i],CISMR$upper[i]), col=col, lty=lty)
  points(i, nc$SMR[i], pch=18, col=col)
}


# -->
# Anson county, which has been pointed out as a clear extreme value in previous studies (Cressie and Chan),
# is the one with the highest confidence interval.
