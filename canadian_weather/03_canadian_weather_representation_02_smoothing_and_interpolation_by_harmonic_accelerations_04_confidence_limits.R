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
# Smoothing with minimizing value of lambda
# ------------------------------------------------------------------------------

# set best lambda
lambda      = 1e6

fdParobj    = fdPar(daybasis, harmaccelLfd, lambda)

logprec.fit = smooth.basis(day.5, logprecav, fdParobj)

logprec.fd  = logprec.fit$fd

fdnames     = list("Day (July 1 to June 30)",
                   "Weather Station" = CanadianWeather$place,
                   "Log 10 Precipitation (mm)")


logprec.fd$fdnames = fdnames



plot(logprec.fd, lwd=2)




# ------------------------------------------------------------------------------
# Estimate Var(y)
#   - Var(y) is assumed to be diagonal. Consequently we need only estimate the variance of the residuals across weather stations for each day.
#     We do this by smoothing the log of the mean square residuals and then exponentiating the result.
# ------------------------------------------------------------------------------

logprecmat  = eval.fd(day.5, logprec.fd)

logprecres  = logprecav - logprecmat

logprecvar  = apply(logprecres^2, 1, sum) / (35 - 1)



# ----------
# smooth log variance vector

lambda      = 1e8

resfdParobj = fdPar(daybasis, harmaccelLfd, lambda)

logvar.fd   = smooth.basis(day.5, log(logprecvar), resfdParobj)$fd



# ----------
# evaluate the exponentiated log variance vector and
# set up diagonal error variance matrix SigmaE
varvec      = exp(eval.fd(day.5, logvar.fd))

SigmaE      = diag(as.vector(varvec))


SigmaE



# ------------------------------------------------------------------------------
# Compute variance-covariance matrix for a functionla probe and extract standard error
# ------------------------------------------------------------------------------

# y2cMap:  converts the raw data vector y to the coefficient vector c of the basis function expansion of x
y2cMap        = logprec.fit$y2cMap


# variance-covariance matrix for a regression coefficients
Sigmachat     = y2cMap %*% SigmaE %*% t(y2cMap)


# c2rMap:  converts the coefficient vector c to the scalar quantity rho(x)
# rho(x) = y2rMap = c2rMap y2cMap
# rho(x) is called "probe", tool for highlighting specific variation, variably weighted linear combinations of function values
# rho(x) = y2rMap  converts a data vector y directly into the probe value
c2rMap        = eval.basis(day.5, daybasis)


# Variance-covariance matrix for a functional probe
Sigmayhat     = c2rMap %*% Sigmachat %*% t(c2rMap)



# ----------
# extract standard error function for yhat
logprec.stderr = sqrt(diag(Sigmayhat))



# ------------------------------------------------------------------------------
# Plot confidence limits for Prince Rupert, British Columbia
# ------------------------------------------------------------------------------

# 29:  Prince Rupert
logprec29 = eval.fd(day.5, logprec.fd[29])


plot(logprec.fd[29], lwd=2, ylim=c(0.2, 1.3))
lines(day.5, logprec29 + 2 * logprec.stderr, lty=2, lwd=2)
lines(day.5, logprec29 - 2 * logprec.stderr, lty=2, lwd=2)
points(day.5, logprecav[,29])

