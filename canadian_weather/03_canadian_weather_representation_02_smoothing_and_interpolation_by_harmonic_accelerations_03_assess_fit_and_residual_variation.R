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
# Residuals for each station
# ------------------------------------------------------------------------------

logprecmat = eval.fd(day.5, logprec.fd)


# residuals
( logprecres = logprecav - logprecmat )




# ----------
# check for sampled station

par(mfrow = c(1,1))

k <- 2

id <- sample(dim(logprecres)[2], size = k, replace = FALSE)

matplot(logprecres[,id], type = "l")




# ------------------------------------------------------------------------------
# Residuals variations (standard deviations) changes over stations 
# ------------------------------------------------------------------------------

# Variance vectors across time of length 35, dividing by 365 - 12;
# the number 12 here is essentially the equivalent degrees of freedom in the fit (logprec.fit$df)
logprec.fit$df

logprecvar2 = apply(logprecres^2, 2, sum) / (365 - 12)



# ----------
rt = which(CanadianWeather$place %in% c("Winnipeg", 'Regina', 'Churchill', 'Montreal', 'St. Johns'))

lft = which(CanadianWeather$place %in%
              c('Yellowknife', 'Resolute', 'Vancouver', 'Iqaluit', 'Pr. George', 'Pr. Rupert') )

below = which(CanadianWeather$place %in% 'Edmonton')

top = which(CanadianWeather$place %in% 'Halifax')



# ----------
# Residual standard deviations changes over stations
# We labels on a few well-known stations and recalling that we number the stations from east to west to north
graphics.off()
par(mfrow=c(1,1))

plot(sqrt(logprecvar2), xlab='Station Number', ylab='Standard Deviation across Day', pch = 20)
text(rt, sqrt(logprecvar2[rt]), labels=CanadianWeather$place[rt], pos=4)
text(lft, sqrt(logprecvar2[lft]), labels=CanadianWeather$place[lft], pos=2)
text(below, sqrt(logprecvar2[below]), labels=CanadianWeather$place[below], pos=1)
text(top, sqrt(logprecvar2[top]), labels=CanadianWeather$place[top], pos=3)



# -->
# We see that there tends to be more variation for prairie and northerly stations in the center of the country
# and less for marine stations.



# ------------------------------------------------------------------------------
# Residual standard deviations taken over stations and within days
# ------------------------------------------------------------------------------

# variance vector stations of length 365, dividing by 35 since the residuals need not sum to zero on any day.
logprecvar1 = apply(logprecres^2, 1, sum) / 35



# ----------
# The smooth line in the plot is computed by smoothing the log of the standard deviations and exponentiating the result
logstddev.fit = smooth.basis(day.5, log(logprecvar1)/2, fdParobj)

logstddev.fd = logstddev.fit$fd

logprecvar1fit = exp(eval.fd(day.5, logstddev.fd))



# ----------
# Residual standard deviations taken over stations and within days
graphics.off()
par(mfrow=c(1,1))

plot(day.5, sqrt(logprecvar1), xlab='Day', ylab='Standard Deviation across Stations', pch = 20)

lines(day.5, logprecvar1fit, lwd=2, col = "blue")



# -->
# We could also have used smooth.pos to do the job.
# We see now that there is a seasonal variation in the size of the residuals, with more variation in summer months than in winter.

# Nevertheless, this form of variation is not strong enough to justify returning to do a weighted least-squares analysis using smooth.basis

# Also implicit in our smoothing technology is the assumption that residuals are uncorrelated.
# This is a rather unlikely situation; departures from smooth variation tend also to be smooth,
# implying a strong positive autocorrelation between neighboring residuals.
# If observation times are equally spaced, we can use standatd time series techniques to explore this autocorrelation structure ...

