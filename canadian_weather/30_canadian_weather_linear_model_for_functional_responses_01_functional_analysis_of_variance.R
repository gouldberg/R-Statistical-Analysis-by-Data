setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  canadian weather
# ------------------------------------------------------------------------------

data("CanadianWeather", package = "fda")

str(CanadianWeather)



# ----------
CanadianWeather$dailyAv[,,'Temperature.C']

CanadianWeather$dailyAv[,,'Precipitation.mm']

CanadianWeather$dailyAv[,,'log10precip']

CanadianWeather$place

CanadianWeather$province

CanadianWeather$coordinates

CanadianWeather$monthlyTemp

CanadianWeather$monthlyPrecip




# ------------------------------------------------------------------------------
# Functional analysis of variance:  climate region effects on temperature
#   - We can divide the weather stations into four distinct gourps: Atlantic, Pacific, Prairie and Arctic.
#     It may be interesting to know the effect of geographic location on the shape of the temperature curves.
#   - In this setup, the intercept term is effectively the Canadian mean temperature curve, and each of the remaining regression coefficients
#     is the perturvation of the Canadian mean required to fit a region's mean temperature.
# ------------------------------------------------------------------------------

# Create a list containing five indicator variables for the intercept term and each of the regions.

regions         = unique(CanadianWeather$region)

p                = length(regions) + 1

regionList       = vector("list", p)

names(regionList)= c('Canada', regions)

regionList[[1]]  = c(rep(1, 35),0)

for (j in 2:p) {
  xj             = (CanadianWeather$region == regions[j-1])
  regionList[[j]]= c(xj, 1)
}


regionList



# ----------
# smoothed temperature with roughness penalty
Lcoef        = c(0,(2*pi/365)^2,0)
harmaccelLfd = vec2Lfd(Lcoef, c(0,365))

tempbasis    = create.fourier.basis(c(0, 365), 65)

lambda       = 1e6
tempfdPar65  = fdPar(tempbasis, harmaccelLfd, lambda)

dayOfYearShifted = c(182:365, 1:181)
tempShifted  = daily$tempav[dayOfYearShifted, ]

tempSmooth65 = smooth.basis(day.5, tempShifted, tempfdPar65)
tempfd       = tempSmooth65$fd



# ----------
# augment tempfd by adding a 36th observation with temp(t) = 0
coef    = tempfd$coef

coef36  = cbind(coef,matrix(0, 65, 1))

temp36fd = fd(coef36, tempbasis, tempfd$fdnames)



# ----------
# Create functional parameter objects for each of the coefficient functions, using 11 Fourier basis functions for each.
betabasis      = create.fourier.basis(c(0, 365), 11)

betafdPar      = fdPar(betabasis)

betaList       = vector("list",p)

names(betaList)= regions

for (j in 1:p) betaList[[j]] = betafdPar



# ----------
# carry out the functional analysis of variance
fRegressList = fRegress(temp36fd, regionList, betaList)



# ----------
# extract the estimated regression coefficients and y-values
betaestList = fRegressList$betaestlist

regionFit   = fRegressList$yhatfd

regions     = c("Canada", regions)




# ------------------------------------------------------------------------------
# The regression coefficients estimated for predicting temperature from climate region.
# ------------------------------------------------------------------------------

graphics.off()

op          = par(mfrow=c(2,3), cex=1)

for (j in 1:p) plot(betaestList[[j]]$fd, lwd=2, xlab="Day (July 1 to June 30)", ylab="", main=regions[j])

plot(regionFit, lwd=2, col=1:4, lty=1:4, xlab="Day (July 1 to June 30)", ylab="", main="Prediction")

par(op)



# -->
# 1st panel: intercept coefficient, corresponding to the Canadian mean temperature
# The last panel contains the predicted mean temperatures for the 4 regions

# Others are the regression coefficients estimated for predicting temperature from climate region.

