setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)


car::some(econ5)




# ----------
# data transformation
# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))

Gi <- resid(lm(log(econ5$govinv) ~ time(log(econ5$govinv))))

Pi <- resid(lm(log(econ5$prinv) ~ time(log(econ5$prinv))))


x <- cbind(G, U, C, Gi, Pi)



# ----------
Ud <- diff(econ5$unemp)
# Ud <- diff(U)

Gd <- diff(econ5$govinv)
# Gd <- diff(G)

Cd <- diff(econ5$consum)
# Cd <- diff(C)

Gid <- diff(econ5$govinv)
# Cid <- diff(Gi)

Pid <- diff(econ5$prinv)
# Pid <- diff(Pi)


dat <- cbind(Gd, Ud, Cd, Gid, Pid)


# dat <- scale(dat)
# Ud <- dat[,"Ud"]
# Gd <- dat[,"Gd"]
# Gid <- dat[,"Gid"]
# Pid <- dat[,"Pid"]




# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# We use m = 3 for both times
# This yields Lh = 1 / weights^2 = 1 / { h(-m)^2 + h(-m+1)^2 ...+ h(m-1)^2 + h(m)^2 } = 9.232, close to the value of L = 9

# The bandwidth = 9.232 / 160 = 0.0577
# modified degrees of freedom is 2 * Lh * 160 / 160 = 18.464  --> 16.5407 ??

# obtain coefficients of modefied Daniell kernel

( ker <- kernel("modified.daniell", c(3,3)) )

plot(ker)



# ----------
# Lh = 1 / sum(hk^2)  k = -6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6  --> 9.24, which is close to the value of L = 9
1 / sum(ker$coef[c(6,5,4,3,2,1,2,3,4,5,6)]^2)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.25:  tapering the upper and lower 25% of the data

graphics.off()

par(mfrow=c(3,2))

Ud.smo <- astsa::mvspec(Ud, kernel = ker, taper = 0.25, log = "no")
abline(v = c(0.025, 0.05, 0.25), lty = 1, col = "blue")

Gd.smo <- astsa::mvspec(Gd, kernel = ker, taper = 0.25, log = "no")
abline(v = c(0.025, 0.05, 0.25), lty = 1, col = "blue")

Cd.smo <- astsa::mvspec(Cd, kernel = ker, taper = 0.25, log = "no")
abline(v = c(0.025, 0.05, 0.25), lty = 1, col = "blue")

Gid.smo <- astsa::mvspec(Gid, kernel = ker, taper = 0.25, log = "no")
abline(v = c(0.025, 0.05, 0.25), lty = 1, col = "blue")

Pid.smo <- astsa::mvspec(Pid, kernel = ker, taper = 0.25, log = "no")
abline(v = c(0.025, 0.05, 0.25), lty = 1, col = "blue")



# -->
# interesting feature is that
# frequency around at 0.25 (= 1 quarter / 0.25 = 4 quarters / cycle) has lack of spectral power.
# This indicates that the data have been seasonally adjusted.
# In addition, because of the seasonal adjustment, some spectral power appears near the seasonal frequency
# This is a distortion apparently caused by the method of seasonally adjusting the data.

# Also we note the spectral power near 0.05 ( = 160 * 0.05 = 8 quarters / cycle),
# in unemployment, GNP, consumption, and, to lesser degree, in private investment.

# Finally, spectral power appears near frequency 0.25 (1 cycle every 8 years) in goverment investment,
# and perhaps to lesser degrees in unemployment, GNP, and consumption





# ------------------------------------------------------------------------------
# Modified degrees of freedom
# ------------------------------------------------------------------------------

# df:  modified degrees of freedom = 2 * Lh * 160 / 480 = 18.464  ??

( df <- Ud.smo$df )


