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
# Scalar response model by functional principal components
# ------------------------------------------------------------------------------

# We resmooth the data using a saturated basis with a roughness penalty
# This represents rather more smoothing than earlier version
daybasis365   = create.fourier.basis(c(0, 365), 365)

lambda        = 1e6

tempfdPar365  = fdPar(daybasis365, harmaccelLfd, lambda)

tempSmooth365 = smooth.basis(day.5, daily$tempav, tempfdPar365)

tempfd = tempSmooth365$fd




# ----------
# Principal components analysis using a roughness penalty
lambda    = 1e0

tempfdPar = fdPar(daybasis365, harmaccelLfd, lambda)

temppca   = pca.fd(tempfd, 4, tempfdPar)

harmonics = temppca$harmonics




# ----------
# linear model using principal component scores
pcamodel = lm(annualprec ~ temppca$scores)

summary(pcamodel)


# -->
# Adjusted R-squared is 0.72, similar to previous analysis



# ----------
# Construct the corresponding functional data objects for regression functions
pcacoefs = summary(pcamodel)$coef

betafd   = pcacoefs[2,1] * harmonics[1] + pcacoefs[3,1] * harmonics[2] + pcacoefs[4,1] * harmonics[3]

coefvar  = pcacoefs[,2]^2

betavar  = coefvar[2] * harmonics[1]^2 + coefvar[3] * harmonics[2]^2 + coefvar[4] * harmonics[3]^2



# ----------
plot(betafd, xlab="Day", ylab="Regression Coef.", ylim=c(-6e-4,1.2e-03), lwd=2)

lines(betafd + 2 * sqrt(betavar), lty=2, lwd=1)

lines(betafd - 2 * sqrt(betavar), lty=2, lwd=1)


