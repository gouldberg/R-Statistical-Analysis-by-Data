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
nc$nwprop <- nc$NWBIR74/nc$BIR74



# ------------------------------------------------------------------------------
# Chi-square test can be carried out to test for (global) significant differences between O and theta * E
#
#   - The statistic is sum( (O(i) ^- theta * E(i))^2 / (theta * E(i)) )
#     where theta is the global SMR = sum(O(i)) / sum(E(i)) and, asymptotically, it follows a chi-square distribution with n degrees of freedom.
#   - If internal standardisation has been used to obtain E(i), then theta is equal to one and the number of defrees of freedom are reduced to n - 1
#     because the additional constraint sum(O(i)) = sum(E(i)) holds.
# ------------------------------------------------------------------------------
library(DCluster)

set.seed(1)

# the asymptotic distribution of the test statistic is a chi-square with n - 1 degrees of freedom and
# an exact test can be done instead of re-sampling
chtest <- achisq.test(Observed ~ offset(log(Expected)), as(nc, "data.frame"), "multinom", 999)

chtest


# ----------
1- pchisq(chtest$t0, 100-1)



# ------------------------------------------------------------------------------
# Potthoff and Whittinghill proposed another test of homogeneity of the means of different Poisson distributed variables
# which can be used to test the homogeneity of the relative risks
#
#  - Althernative hypothesis is that the relative risks are drawn from a gamma distribution with mean lambda and variance simga^2
#    and O(i) are distributed following a Negative Binomial distribution.
#  - This test can also be considered as a test of over-dispersion.
# ------------------------------------------------------------------------------
set.seed(1)

pwtest <- pottwhitt.test(Observed ~ offset(log(Expected)), as(nc, "data.frame"), "multinom", 999)

pwtest



# ----------
# The asymptotic distribution of this statistic is Normal with mean O(+) * (O(+) - 1) and
# variance 2 * n * O(+) * (O(+) - 1)), so a one-side test can be done as follows:
Oplus<- sum(nc$Observed)

1 - pnorm(pwtest$t0, Oplus*(Oplus-1), sqrt(2 * 100 * Oplus * (Oplus - 1)))




# ------------------------------------------------------------------------------
# Cressie and Read already mentioned that the Poisson model was not appropriate for the SIDS data
# due to the presence of over-dispersion and that other models that take it into account would be more appropriate.

# In case of doubt. it is advised to assess the significance of a given test by using the Multinomial distribution.
# This is the standard procedure to assess the significance of the test statistic by Monte Carlo in this scenario.

