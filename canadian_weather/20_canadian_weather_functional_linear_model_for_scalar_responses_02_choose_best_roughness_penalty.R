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
# Choosing Smoothing Parameters
# ------------------------------------------------------------------------------

# set up a harmonic acceleration operator
Lcoef = c(0, (2 * pi / 365)^2, 0)

harmaccelLfd = vec2Lfd(Lcoef, c(0, 365))



# ----------
conbasis   = create.constant.basis(c(0, 365))

betabasis35 = create.fourier.basis(c(0, 365), 35)
betafdPar  = fdPar(betabasis35, harmaccelLfd)



# ----------
betalist  = vector("list",2)
betalist[[1]] = conbasis
betalist[[2]] = betafdPar



# ----------
loglam = seq(5, 15, 0.5)

nlam   = length(loglam)

SSE.CV = rep(NA, nlam)

for (ilam in 1:nlam) {
  print(paste("log lambda =", loglam[ilam]))
  
  betalisti  = betalist
  
  betafdPar2 = betalisti[[2]]
  
  betafdPar2$lambda = 10^(loglam[ilam])
  
  betalisti[[2]] = betafdPar2
  
  fRegi          = fRegress.CV(annualprec, templist, betalisti)
  SSE.CV[ilam]   = fRegi$SSE.CV
}



# ----------
plot(loglam, SSE.CV, type="b", lwd=2,
     xlab="log smoothing parameter lambda",
     ylab="Cross-validation score", cex=1.2)


# -->
# best log smoothing parameter lambda is 12.5
