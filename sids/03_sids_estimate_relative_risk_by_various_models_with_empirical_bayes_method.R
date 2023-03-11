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
# Empirical Bayes (EB) estimates for the relative risk (Poisson-Gamma model)
#   - This estimator may not converged in some circumstances
# ------------------------------------------------------------------------------
library(DCluster)

eb <- empbaysmooth(nc$Observed, nc$Expected)

nc$EBPG <- eb$smthrr

eb$nu
eb$alpha



# ----------
# ebnu <- eb$nu
# ebalpha <- eb$alpha

nc$pvalpois <- ppois(nc$Observed, nc$Expected, lower.tail=FALSE)

nbparam <- calculate.mle(as(nc, "data.frame"), model="negbin")
nc$pvalnegbin <- pnbinom(nc$Observed, size=nbparam$size, prob=nbparam$prob, lower.tail=FALSE)


colorkeypval <- list(labels=as.character(c(0, 0.01, 0.05, 0.1, .5, 1)), at=(0:5)/5, height=.5)
pvalcols <- brewer.pal(5, "Reds")
print(spplot(nc, c("pvalpois","pvalnegbin"), col.regions=rev(pvalcols), 
             at=c(0, 0.01, 0.05, 0.1, .5, 1), axes=TRUE))



# ------------------------------------------------------------------------------
# Log-normal Model
#   - Clayton and Kaldor (1987) proposed another risk estimator based on assuming that
#     the logarithm of the relative risks (theta(i)) follows a multivariate normal distribution with common mean and variance.
#   - In this case, the estimate of the log-relative risk is not taken as log(O/E) but log((O+1/2)/E), because the former is not defined if O is zero.
#   - The EM algorithm is used to obtain estimates of the mean and variance of the model, which can be plugged into the Empirical Bayes estimator of beta.
#     The estimator for relative risk (theta(i) = exp(beta(i))
#   - This estimator based on the log-Normal model seems to be the most computationally stable and reliable
# ------------------------------------------------------------------------------

ebln <- lognormalEB(nc$Observed, nc$Expected)

( nc$EBLN <- exp(ebln$smthrr) )



# ------------------------------------------------------------------------------
# Marshall's Global EB Estimator
#   - Marshall developed a new EB estimator assuming that the relative risks (theta(i)) have a common prior mean (mu) and variance (sigma^2),
#     but specifying any distribution. By using the Method of Moments, he is able to work a new estimator out employing a shrinkage estimator
#     theta(i) = mu-hat + C(i) (SMR(i) - mu-hat), where mu-hat = sum(O(i)) / sum(E(i))
#   - This estimator can be unfeasible in some circumstances.
# ------------------------------------------------------------------------------
library(spdep)

EBMarshall <- EBest(nc$Observed, nc$Expected)

( nc$EBMarshall <- EBMarshall[,2] )



# ------------------------------------------------------------------------------
# Compariaon of different risk estimators
# ------------------------------------------------------------------------------
atcol <- (0:5) * max(nc$SMR) / 5


# colorkey <- list(labels=as.character(c(formatC(brks, format="f", dig=2))), at=atcol,  height=.5)

print(spplot(nc, c("SMR", "EBPG", "EBLN", "EBMarshall"), col.regions = cols, at = brks, axes = TRUE))


# -->
# All EB estimators seem to produce very similar estimates in all the areas.
# SMR's values are smoothed by taking into account global information in the computation of the estimate.
# The SMR is the most variable and that the other 3 have been shrunk towards the glabal mean, which is approximately 1.


