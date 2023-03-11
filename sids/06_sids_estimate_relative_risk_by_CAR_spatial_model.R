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



# ------------------------------------------------------------------------------
# set colors
# ------------------------------------------------------------------------------
library(RColorBrewer)

logSMR <- log(nc$SMR[nc$SMR > 0])
nsteps <- 7
step <- (max(logSMR) - min(logSMR)) / nsteps
brks <- exp(min(logSMR) + (0:nsteps) * step)
brks[1] <- 0
cols <- c(rev(brewer.pal(3, "Blues")), brewer.pal(4, "Reds"))



# ------------------------------------------------------------------------------
# Important covariate
# ------------------------------------------------------------------------------
# Cressie and Chan (1989) considered the proportion of non-white births as an important factor related to the incidence of SIDS
nc$RATIO <- nc$NWBIR74/nc$BIR74

print(spplot(nc, "RATIO", col.regions=brewer.pal(4, "Reds"), at=.8*0:4/4))


# -->
# Notice how there exists a similar pattern to that shown by the spatial distribution of the SMR and the different EB estimates.



# ------------------------------------------------------------------------------
# Spatial CAR model by R2BayesX
# ------------------------------------------------------------------------------

library(R2BayesX) 

nc$nwprop <- nc$NWBIR74/nc$BIR74


# ----------
# nb2gra will convert from a nb object into a gra object, which is the one required by function sx to include spatial adjacency.
ncgra <- nb2gra(ncCR85)

# sx(AREAID, bs="re"): include independent Gaussian random effects
# sx(FIPSNO, bs="spatial", map=ncgra): include spatial random effects
bymbayesx <- bayesx(Observed ~ nwprop + sx(AREAID, bs="re") + sx(FIPSNO, bs="spatial", map=ncgra),
                    offset= log(nc$Expected),
                    family="poisson", data=as(nc, "data.frame"))


summary(bymbayesx)


nc$BAYESX <- bymbayesx$fitted.values[order(bymbayesx$bayesx.setup$order),2] / nc$Expected



# ------------------------------------------------------------------------------
# Spatial CAR model by INLA
# ------------------------------------------------------------------------------
library(INLA)
inla.version()


# f(FIPS, model = "iid"): include independent Gaussian random effects
# f(AREAID, model = "besag", graph = nb2mat(ncCR85, style="B")):  include spatial random effects
INLA_BYM <- inla(Observed ~ nwprop + f(FIPS, model="iid") + f(AREAID, model = "besag", graph = nb2mat(ncCR85, style="B")) + offset(log(Expected)), 
                 family="poisson", data=as(nc, "data.frame"), control.predictor=list(compute=TRUE))


summary(INLA_BYM)

nc$INLA <- INLA_BYM$summary.fitted.values[,1] / nc$Expected



# ------------------------------------------------------------------------------
# Spatial CAR model by CARBayes
# ------------------------------------------------------------------------------
library(CARBayes)


set.seed(1)
obj <- S.CARbym(Observed ~ nwprop + offset(log(Expected)), data=nc, family="poisson", W=nb2mat(ncCR85, style="B"), n.sample=30000, burnin=20000, thin=10, verbose=FALSE)

print(obj)

nc$CARBayes <- obj$fitted.values / nc$Expected



# ------------------------------------------------------------------------------
# Compare models:  posterior means of the relative risks
# ------------------------------------------------------------------------------
print(spplot(nc, c("BAYESX", "INLA", "CARBayes"), at=brks, axes=TRUE, col.regions=cols))



# ------------------------------------------------------------------------------
# Compare models:  posterior marginals of the fixed effects and posterior means of the spatial and non-spatial random effects
# ------------------------------------------------------------------------------
oldpar <- par(mfrow=c(2,2))


# ----------
# Intercept
d1 <- density(attr(bymbayesx$fixed.effects, "sample")[,1])
plot(as.data.frame(d1[1:2]), main="Intercept", type="l")
lines(INLA_BYM$marginals.fixed[[1]], lty=2)
lines(density( obj$samples$beta[,1], bw=d1$bw), lty=3) 
legend("topleft", legend=c("BayesX", "INLA", "CARBayes"), bty="n", lty=1:3, cex=.65)



# ----------
# Nwprop
d2 <- density(attr(bymbayesx$fixed.effects, "sample")[,2])
plot(as.data.frame(d2[1:2]), main="nwprop", type="l")
lines(INLA_BYM$marginals.fixed[[2]], lty=2)
lines(density( obj$samples$beta[,2], bw=d2$bw), lty=3) 
legend("topleft", legend=c("BayesX", "INLA", "CARBayes"), bty="n", lty=1:3, cex=.65)



# ----------
# FIXME: Add variances here
# Density of posterio means of non-spatial random effects
sxname <- names(bymbayesx$effects)[grep("sx\\(AREAID\\)", names(bymbayesx$effects))]
d3 <- density(bymbayesx$effects[[sxname]]$Mean)
plot(as.data.frame(d3[1:2]), main="Non-spatial r.eff.", type="l")
lines(density(INLA_BYM$summary.random$FIPS$mean, bw=d3$bw), lty=2)
# from CARBayes 4.0 only single re
lines(density( apply( obj$samples$psi, 2, mean), bw=d3$bw), lty=3) 
# lines(density(MCMCres$mean$u, bw=d3$bw), lty=4)
legend("topleft", legend=c("BayesX", "INLA", "CARBayes"),  bty="n", lty=1:3, cex=.65)


# ----------
# Variance of spatial random effects
# FIXME: check the prior that INLA uses for this. Seems too smoothed
sxname <- names(bymbayesx$effects)[grep("sx\\(FIPSNO\\)", names(bymbayesx$effects))]
d4 <- density(bymbayesx$effects[[sxname]]$Mean)
plot(as.data.frame(d4[1:2]), main="Spatial r.eff.", type="l")
lines(density(INLA_BYM$summary.random$AREAID$mean, bw=d4$bw), lty=2)
# from CARBayes 4.0 only single re
lines(density( apply( obj$samples$theta, 2, mean), bw=d4$bw), lty=3) 
# lines(density(MCMCres$mean$v, bw=d4$bw), lty=4)
legend("topleft", legend=c("BayesX", "INLA", "CARBayes"), bty="n", lty=1:3, cex=.65)


# ----------
par(oldpar)


