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
# Data of ReginaPrecip
# ------------------------------------------------------------------------------

# daily precipitation for the Canadian prairie city of Regina over the month of June and over the 34 years from 1960 through 1993.

plot(ReginaPrecip, type = "l", lty = 1)


# -->
# June is the critical month for wheat growers because the crop enters its most rapid growing phase, and an adequate supply of moisture in the soil
# is essential for a good crop.



# ----------
# ordered rainfalls for the 1,006 days when precipitation was recorded against their rank orders, a version of quantile plot.
NR = length(ReginaPrecip)

plot(1:NR, sort(ReginaPrecip), xlab='Rank of rainfall',
     ylab='Ordered daily rainfall (mm)' )


# -->
# The highest rainfall of 132.6 mm on June 25, 1975, is said to have flooded 20,000 basements. 



# ------------------------------------------------------------------------------
# Evaluate variation by estimated probability density function
# ------------------------------------------------------------------------------
# Precipitation is a difficult qunatity to model for several reasons.
#   - On about 65% of the days in this region, no rain is even possible, so that zero really means a "nonprecipitation day" rather than "no rain".
#     Since there can be a small amount of precipitation from dew, we used only days when the measured precipitation exceeded two millimeteres.
#   - Precipitation can come down in two main ways: as a gentle drizzle and, more often, as a sudden and sometimes violent thunderstorm.
#     Consequently, the distribution of precipitation is extremely skewed, and Regina experienced 3 days in this period with more than 40 mm of rain.
#     We deleted these days, too, in order to improve the graphical displays, leaving N = 212 rainfall values.

# RegPrec contains the 212 sorted rainfall amounts between 2 and 45 mm
sel2.45 = ((2 <= ReginaPrecip) & (ReginaPrecip <= 45))

RegPrec = sort(ReginaPrecip[sel2.45])

( N = length(RegPrec) )



# ----------
# We set up the break points for a cubic B-splie basis to be the rainfalls at 11 equallly scaped ranks
( Wknots  = RegPrec[round(N * seq(1 / N, 1, len=11), 0)] )

( Wnbasis = length(Wknots) + 2 )



# ----------
Wbasis  = create.bspline.basis(rangeval = range(RegPrec), nbasis = Wnbasis, norder = 4, breaks = Wknots)

Wbasis



# ----------
# set up the functional parameter object with light amount of smoothing
Wlambda     = 1e-1

WfdPar      = fdPar(fdobj = Wbasis, Lfdobj = 2, lambda = Wlambda)

WfdPar$fd$coefs



# ----------
# estimate the density
# Function density.fd is used to estimate a nonparametric probability density function from a sample of data.

# --> ERROR !!!
densityList = density.fd(RegPrec, WfdPar)


# functional data object
Wfd         = densityList$Wfdobj

# normalizing constant C
C.          = densityList$C



# ----------
# plot the estimated density with the knot locations
Zfine = seq(RegPrec[1], RegPrec[N], len=201)

Wfine = eval.fd(Zfine, Wfd)

Pfine = exp(Wfine)/C.

plot(Zfine, Pfine, type='l', lwd=2, xlab='Precipitation (mm)', ylab='Probability Density')
abline(v=Wknots, lty='dashed', lwd=2)



# -->
# The vertical dashed lines indicate the knot values used to define the cubic B-spline expansion for w = lnP

# The multiphase nature of precipitaion is clear here.
# The 1st phase is due to heavy dew or a few drops of rain, followed by a peak related to light rain from low-pressure ridges that arrive in this area
# from time to time, and then thunderstorm rain that can vary from about 7mm to catastrophic levels.
