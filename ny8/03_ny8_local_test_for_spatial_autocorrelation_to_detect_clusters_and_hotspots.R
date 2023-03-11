setwd("//media//kswada//MyFiles//R//ny8")

packages <- c("dplyr", "maptools", "spdep")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ny8
# ------------------------------------------------------------------------------
library(rgdal)
NY8 <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8_utm18.shp", "NY8_utm18")
TCE <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//TCE.shp", "TCE")

library(spdep)
NY_nb <- read.gal("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY_nb.gal", region.id = row.names(NY8))
cities <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8cities.shp", "NY8cities")



# ----------
# subsetting only Syracuse city
Syracuse <- NY8[NY8$AREANAME == "Syracuse city",]
Sy0_nb <- subset(NY_nb, NY8$AREANAME == "Syracuse city")

summary(Sy0_nb)



# ------------------------------------------------------------------------------
# Moran scatterplot
# ------------------------------------------------------------------------------
# Moran scatterplot:
#  - x-axis:  leukaemia case caunt
#  - y-axis:  the spatially weighted sum of values of neighbours -- the spatially lagged values
# Global Moran7s I is a linear relationship between these and is drawn as a slope
# The plot is further partitioned into quadrants at the mean values of the variable and its lagged values.

par(mfrow=c(1,1))
moran.plot(NY8$Cases, listw=nb2listw(NY_nb, style="C"), quiet=TRUE)
title("Moran scatterplot")



# ------------------------------------------------------------------------------
# Detecting observations with unusually strong influence on the slope
# ------------------------------------------------------------------------------
# moran.plot calls influence.measures on the linear model of lm(wx ~ x) providing the slope coeffs, where wx is the spatially lagged value of x.
# This means that we can see whether particular local relationsips are able to influence the slope more than proportionally.
msp <- moran.plot(NY8$Cases, listw=nb2listw(NY_nb, style="C"), quiet=TRUE)

infl <- apply(msp$is.inf, 1, any)
x <- NY8$Cases
lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), labels=c("L", "H"), include.lowest=TRUE)
wx <- stats::lag(nb2listw(NY_nb, style="C"), NY8$Cases)
lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), labels=c("L", "H"), include.lowest=TRUE)
lhlh <- interaction(lhx, lhwx, infl, drop=TRUE)

cols <- rep(1, length(lhlh))
cols[lhlh == "H.L.TRUE"] <- 2
cols[lhlh == "L.H.TRUE"] <- 3
cols[lhlh == "H.H.TRUE"] <- 4

plot(NY8, col=brewer.pal(4, "Accent")[cols])
legend("topright", legend=c("None", "HL", "LH", "HH"), fill=brewer.pal(4, "Accent"), bty="n", cex=0.8, y.intersp=0.8)
title("Tracts with influence")



# ------------------------------------------------------------------------------
# Local Moran's I
#   - values are constructed as the n components summed to reach global Moran's I
# ------------------------------------------------------------------------------
( lm1 <- localmoran(NY8$Cases, listw=nb2listw(NY_nb, style="C")) )

head(lm1)

NY8$Standard <- lm1[,1]
NY8$Randomisation <- lm1[,5]



# ----------
# Saddlepoint approximations and exact methods can be of importance because the number of neighbours of each observation
# is very small, and this in turn may make the adoption of the normality assumption problematic.
( lm2 <- as.data.frame(localmoran.sad(lm(Cases ~ 1, NY8), nb=NY_nb, style="C")) )

head(lm2)

NY8$Normal <- lm2[,3]
NY8$Saddlepoint <- lm2[,5]



( lm3 <- as.data.frame(localmoran.exact(lm(Cases ~ 1, NY8), nb=NY_nb, style="C")) )

head(lm3)

NY8$Exact <- lm3[,5]



# ------------------------------------------------------------------------------
# Local Moran's I
# based on Waller and Gotway (2004) Poisson assumption for the constant risk hypothesis
# ------------------------------------------------------------------------------
r <- sum(NY8$Cases) / sum(NY8$POP8)
rni <- r * NY8$POP8
lw <- nb2listw(NY_nb, style="C")
sdCR <- (NY8$Cases - rni)/sqrt(rni)
wsdCR <- stats::lag(lw, sdCR)
I_CR <- sdCR * wsdCR

NY8$"Constant_risk" <- I_CR



# ----------
# Local Morans's I values calculated directly and using the constant risk hypothesis.
library(RColorBrewer)
gry <- c(rev(brewer.pal(8, "Reds")[1:6]), brewer.pal(6, "Blues"))

# nms <- match(c("Standard", "Constant_risk"), names(NY8))
spplot(NY8, c("Standard", "Constant_risk"), at=c(-2.5,-1.4,-0.6,-0.2,0,0.2,0.6,4,7), col.regions=colorRampPalette(gry)(8))


# -->
# There are some sign changes between the maps, with the constant risk hypothesis values somewhat farther from zero.



# ------------------------------------------------------------------------------
# Local Moran's I
# Monte Carlo test of the constant risk hypothesis local Moran's I values
# ------------------------------------------------------------------------------
# we use a parametric approach to simulating the local counts using the local expected count as the parameter to rpois,
# because the neighbour counts are very low and make permuation unwise.


nsim <- 999

N <- length(rni)

sims <- matrix(0, ncol=nsim, nrow=N)

set.seed(1234)
for (i in 1:nsim) {
  y <- rpois(N, lambda=rni)
  sdCRi <- (y - rni)/sqrt(rni)
  wsdCRi <- stats::lag(lw, sdCRi)
  sims[,i] <- sdCRi * wsdCRi 
}

xrank <- apply(cbind(I_CR, sims), 1, function(x) rank(x)[1])

diff <- nsim - xrank
diff <- ifelse(diff > 0, diff, 0)

( pval <- punif((diff + 1)/(nsim + 1)) )


NY8$Constant_risk <- pval



# ------------------------------------------------------------------------------
# Compare probability values for all census trancts, local Moran's I
# ------------------------------------------------------------------------------
gry <- c(rev(brewer.pal(6, "Reds")), brewer.pal(6, "Blues"))

spplot(NY8, c("Normal", "Randomisation", "Saddlepoint", "Exact", "Constant_risk"), at=c(0,0.01,0.05,0.1,0.9,0.95,0.99,1), col.regions=colorRampPalette(gry)(7))




# ----------
# Zoom in to examine the local Moran's I probability values for 3 calculation methods for the tracts
# in and near the city of Binghampton
spplot(NY8, c("Normal", "Exact", "Constant_risk"), xlim=c(405200, 432200), ylim=c(4652700, 4672000), at=c(0,0.01,0.05,0.1,0.9,0.95,0.99,1), col.regions=colorRampPalette(gry)(7))


# -->
# It appears that the use of the constant risk approach handles the heterogeneity in the counts better than the alternatives.


