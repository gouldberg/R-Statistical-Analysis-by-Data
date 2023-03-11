setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)


# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# We use m = 3 for both times
# This yields Lh = 1 / weights^2 = 1 / { h(-m)^2 + h(-m+1)^2 ...+ h(m-1)^2 + h(m)^2 } = 22.14, close to the value of L = 21
# The bandwidth = 22.14 / 1024 = 0.0216
# modified degrees of freedom is 2 * Lh * 1024 / 1024 = 44.28

# obtain coefficients of modefied Daniell kernel
m <- 3

( ker <- kernel("modified.daniell", c(m, m)) )

plot(ker)



# ----------
# Lh = 1 / sum(hk^2)  --> 22.14, which is close to the value of L = 21
k <- c(seq(m * 2, 1, by = -1), 0, seq(1, m * 2, by = 1))

1 / sum(ker$coef[k]^2)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()

par(mfrow=c(1,2))

lapply(1:5, function(i){
  eqp.smo <- astsa::mvspec(x[,i], kernel = ker, taper = 0.1, log = "no", main = paste0("P : ", x.name[i]))
  
  eqs.smo <- astsa::mvspec(x[,i+5], kernel = ker, taper = 0.1, log = "no", main = paste0("S : ", x.name[i+5]))
})



# -->
# There have been many attempts at dealing with the problem of smoothing the periodogram in a automatic way;
# an early reference is Wahba.
# It is perhaps better to perform automatic adaptive smoothing for estimating the spectrum.
# We refer interested readers to Fan and Kreutzberger and the numerous references within.



# ------------------------------------------------------------------------------
# bandwidth
# ------------------------------------------------------------------------------

# bandwidth = Lh / 1014 = 22.14 / 1024 = 0.0216 ... ??
eqp.smo$bandwidth

eqs.smo$bandwidth



# ------------------------------------------------------------------------------
# Modified degrees of freedom
# ------------------------------------------------------------------------------

# df:  modified degrees of freedom = 2 * Lh * 1024 / 1024 ~ 44.28 ??
( df <- eqp.smo$df )




# ------------------------------------------------------------------------------
# Tapered spectrum and non-tapered spectrum:  tapering effect and leakage
#   - For example, a large dynamic range for the values in the spectrum introduces spectra in contiguous frequency intervals
#     several orders of magnitude greater than the value in the interval of interest.
#     This effect is called "leakage"
# ------------------------------------------------------------------------------

# no taper
s0 <- astsa::mvspec(EQ5[P], spans = c(21, 21), plot=FALSE)


# full taper
s50 <- astsa::mvspec(EQ5[P], spans = c(21, 21), taper = 0.5, plot=FALSE)



# ----------
par(mfrow=c(1,1))

# full taper
plot(s50$freq, s50$spec, log="yes", type = "l", ylab = "spectrum", xlab = "frequency")

# no taper
lines(s0$freq, s0$spec, lty = 2)



# -->
# the tapered spectrum does a better job ...?

