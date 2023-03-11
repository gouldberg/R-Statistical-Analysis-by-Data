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
# Moran's I test for spatial autlcorrelation:  simulation
#  - as a ratio of the product of the variable of interest and its spatial lag, with the cross-product of the variable of interest,
#    and adjusted for the spatial weights used.
# ------------------------------------------------------------------------------
( Sy0_lw_W <- nb2listw(Sy0_nb) )



# ----------
# for comparison, generate uncorrelated vector and correlated vector
n <- length(Sy0_nb)
set.seed(987654)
uncorr_x <- rnorm(n)
rho <- 0.5
( autocorr_x <- invIrW(Sy0_lw_W, rho) %*% uncorr_x )



# ----------
# also for comparison, generate knn2nd(k=2) neighbours list
coords <- coordinates(Syracuse)
IDs <- row.names(Syracuse)
Sy9_nb <- knn2nb(knearneigh(coords, k=2), row.names=IDs)


# ----------
# introducing gentle trend rising from west to east into the uncorrelated random variable
et <- coords[,1] - min(coords[,1])
trend_x <- uncorr_x + 0.00025 * et



# ----------
# Moran's I test

# There is no trace of spatial dependence with these weights
moran.test(uncorr_x, listw=Sy0_lw_W)

# significant result for spatial weights
moran.test(autocorr_x, listw=Sy0_lw_W)

# although it is marginally significant, had the generating process been less strong, we might havve come to the worng conclusion based on the 
# choice of spatial weights not matching the actual geneating process.
moran.test(autocorr_x, listw=nb2listw(Sy9_nb, style="W"))



# ----------
# We can get back to the uncorrelated residuals by including the trend in the mean.
moran.test(trend_x, listw=Sy0_lw_W)
lm.morantest(lm(trend_x ~ et), listw=Sy0_lw_W)




# ------------------------------------------------------------------------------
# Moran's I test for spatial autlcorrelation
#  - the testing the number of cases by census tract for autocorrelation
# ------------------------------------------------------------------------------
K <- moran(NY8$Cases, listw=nb2listw(NY_nb, style="B"), n=length(NY8$Cases), S0=Szero(nb2listw(NY_nb, style="B")))$K



# ----------
# using the default spatial weights style of row standardisation  --> significant
moran.test(NY8$Cases, listw=nb2listw(NY_nb))



# ----------
# using style = "B" spatial weights (make all weights equal and summing to the number of observations)
# --> significant
lw_B <- nb2listw(NY_nb, style="B")
moran.test(NY8$Cases, listw=lw_B)



# ----------
# By default, moran.test uses the randomisation assumption, 
# which differs from the simpler normality assumption by introducting a correction term based on the kurtosis of the variable of interest.
# If the kertosis value departs from normality, the randomisation assumption compensates by increasing the variance and decreasing the standard deviate.
moran.test(NY8$Cases, listw=lw_B, randomisation=FALSE)



# ----------
# standard test under normality is in fact the same test as the Moran test for regression residuals for the model,
# including only the intercept.
lm.morantest(lm(Cases ~ 1, NY8), listw=lw_B)



# ----------
# using a Saddlepoint approximation rather than the analytical normal assumption
# and using exact test
# These methods are substantially more demanding computationally, and were originally regarded as impractical.
# For moderately sized data sets such as the one we are using, however, need less than double the time required for reaching a result.
# In general, exact and Saddlepoint methods make little difference to outcomes for global tests when the number of spatial entities is not small,
# as here, with the probability value only chainging by a factor of 2.
lm.morantest.sad(lm(Cases ~ 1, NY8), listw=lw_B)
lm.morantest.exact(lm(Cases ~ 1, NY8), listw=lw_B)



# ----------
# Monte Carlo test, a permutation bootstrap test
set.seed(1234)
bperm <- moran.mc(NY8$Cases, listw=lw_B, nsim=999)
bperm



# ------------------------------------------------------------------------------
# Parametric bootstrap assessment of the significance of autocorrelation
#   - Waller and Gotway (2004) also include a Poisson constant risk parametric bootstrap assessment of the significance of autocorrelation in the case counts.
# ------------------------------------------------------------------------------
CR <- function(var, mle) rpois(length(var), lambda=mle)
MoranI.pboot <- function(var, i, listw, n, S0, ...) {
  return(moran(x=var, listw=listw, n=n, S0=S0)$I)
}


# ----------
library(boot)

# The constant global rate r is calculated first, and used to create expected counts for each census tract by multiplying by the population.
r <- sum(NY8$Cases)/sum(NY8$POP8)
rni <- r*NY8$POP8


# The expected counts can also be expressed as the fitted values of a null Poisson regression
# with an offset set to the logarithm of tract population - with a log-link
# Because Cases are not all integer, warnings are generated.

# rni <- fitted(glm(Cases ~ 1 + offset(log(POP8)), data=NY8, family="poisson"))

set.seed(1234)
boot2 <- boot(NY8$Cases, statistic=MoranI.pboot, R=999, sim="parametric",
              ran.gen=CR, listw=lw_B, n=length(NY8$Cases), S0=Szero(lw_B), mle=rni)


pnorm((boot2$t0 - mean(boot2$t))/sd(boot2$t[,1]), lower.tail=FALSE)


# --> this is not significant


# ----------
# Histograms of simulated values of Moran's I under random permutaions of the data 
# and parametric samples from constant risk expected values
# The observed value of Moran's I is marked by a vertical line.
oopar <- par(mfrow=c(1,2))
xlim <- range(c(bperm$res, boot2$t[,1]))
hist(bperm$res[-length(bperm$res)], main="Permutation bootstrap", xlab=expression(I[std]), xlim=xlim, density=15, angle=45, ylim=c(0,260))
abline(v=bperm$statistic, lty=2)
hist(boot2$t, col=rgb(0.4,0.4,0.4), main="Parametric bootstrap", xlab=expression(I[CR]), xlim=xlim, ylim=c(0,260))
hist(bperm$res[-length(bperm$res)], density=15, angle=45, add=TRUE)
abline(v=boot2$t0, lty=2)
par(oopar)


# -->
# The parametric simulations shifting the distribution of Moran's I rightwards,
# because it is taking the impact of the heterogeneous tract populations into account.



# ------------------------------------------------------------------------------
# Moran's I adapted to use an Empirical Bayes rate
# that shrinks extreme rates for tracts with small populations at risk
# towards the rate for the area as a whole
# ------------------------------------------------------------------------------
set.seed(1234)

EBImoran.mc(n=NY8$Cases, x=NY8$POP8, listw=nb2listw(NY_nb, style="B"), nsim=999)


# -->
# The results suggest that one reason for the lack of significance of the parametric bootstrapping of the constant risk observed 
# and expected values could be that 
# unusual and extreme values were observed in tracts with small populations.
# Once the rates have been smoothed, some global aautoorrelation is found.



# ------------------------------------------------------------------------------
# spatial correlogram
# ------------------------------------------------------------------------------
cor8 <- sp.correlogram(neighbours=NY_nb, var=NY8$Cases, order=8, method="I", style="C")



# ----------
library(pgirmess)

# the function automatically selects distance bands of almost 10 km, spanning the whole study area.
corD <- correlog(coordinates(NY8), NY8$Cases, method="Moran")



# ----------
oopar <- par(mfrow=c(1,2))
plot(cor8, main="Contiguity lag orders")
plot(corD, main="Distance bands")
par(oopar)


# -->
# left panel: values of Moran's I for eight successive lag orders of contiguous neighbours
# suggesting that second-order neighbours are also positively autocorrelated (although the probability values should be adjusted for multiple comparisons)

# right panel: values of Moran's I for a sequence of distance band neighbour pairs
# suggesting that the first two bands of 0-10 and 10-20 km have significant values

