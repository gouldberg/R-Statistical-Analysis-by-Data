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
#  Poisson-Gamma model as a model with random effects in the loc-scale by R2BayesX
#
#   - Poisson-Gamma model can be regarded as a model with random effects in the log-scale, i.e., the terms log(theta(i)) can be seen
#     as a set of random effects with a common prior distribution.
#       - O(i) ~ Po(E(i) * theta(i))
#       - log(theta(i)) = alpha + u(i)
#       - alpha ~ Normal(0, sigma1^2)
#       - u(i) ~ Normal(0, sigma2^2)
#     where the intercept alpha is included to model the overall risk and random effects u(i) accounts from differences between the areas.
#
#   - BayesX is aimed at fitting structured additive regression models
#   - BayesX has also the possibility of fitting models using MCMC, restricted maximum likelihood (REML) and penalised least squares (PLS).
# ------------------------------------------------------------------------------
library(R2BayesX)



# ----------
# bayesx works similarly to function gam.
# Additive model terms can be included using function sx to set a different random effect for each area.
nc$AREAID <- 1:nrow(nc)

pgbayesx <- bayesx(Observed ~ sx(AREAID, bs="re"), 
                   offset= log(nc$Expected),
                   family = "poisson", data = as(nc, "data.frame"))


nc$PGBAYESX <- pgbayesx$fitted.values[order(pgbayesx$bayesx.setup$order),2]/nc$Expected

print(spplot(nc, c("PGBAYESX"), col.regions=cols,  at=brks, axes = TRUE))



# ------------------------------------------------------------------------------
#  Poisson-Gamma model as a model with random effects in the loc-scale by INLA
#
#    - INLA (the Integrated Nested Laplace Approximation) to approximate the posterior marginals of the parameters in the model.
#      Although INLA will not provide the full posterior distribution, this is not needed in many applications,
#      such as the estimation of relative risks is dease mapping.
#    - INLA provides accurate estimates in most cases at lower computational time.
# ------------------------------------------------------------------------------

library(INLA)
inla.version()


pginla <- inla(Observed ~ offset(log(Expected)) - 1 + f(AREAID, model = "iid"),
               family = "poisson",  data = as(nc, "data.frame"),
               control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE)
)


nc$PGINLA <- pginla$summary.fitted.values$mean/nc$Expected



# ------------------------------------------------------------------------------
#  Poisson-Gamma model as a model with random effects in the loc-scale by CARBayes  --> CURRENTLY NOT AVAILABLE ?
#
#    - CARBayes provide a number of functions for fitting some widely used spatial models using MCMC.
#      Although it is more limited than the previous packages, it is entirely implemented in R and
#      provides a simple alternative to fitting spatial models for disease mapping.
#
#    - S.glm: fit a generalized linear model to data, where the response variable can be binomial, Gaussian, multinomial, Poisson or zero-inflated Poisson (ZIP).
#      Inference is conducted in a Bayesian setting using MCMC simulation.
# ------------------------------------------------------------------------------
library(CARBayes)


set.seed(1)

ncdf <- as(nc, "data.frame")

attach(ncdf)
pgcarbayes <- poisson.independent(formula = Observed ~ offset(log(Expected)), data=nc, family="poisson", burnin=5000, n.sample=10000, verbose=TRUE)
# pgcarbayes <- S.glm(formula = Observed ~ offset(log(Expected)), data=nc, family="poisson", burnin=5000, n.sample=10000, verbose=TRUE)
detach(ncdf)

pgcarbayes


# ----------
nc$PGCARBAYES <- pgcarbayes$fitted.values[,1] / nc$Expected
nc$PGCARBAYES <- pgcarbayes$fitted.values / nc$Expected



# ------------------------------------------------------------------------------
# compare estimation
# ------------------------------------------------------------------------------

graphics.off()
print(spplot(nc, c("PGINLA", "PGBAYESX", "PGCARBAYES"), col.regions=cols,  at=brks, axes = TRUE))





