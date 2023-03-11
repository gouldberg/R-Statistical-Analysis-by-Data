setwd("//media//kswada//MyFiles//R//wing")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  wing
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//wing.ppm")

M


str(M)


# -----------
plot(M)



# ------------------------------------------------------------------------------
# Examine deviation between the reconstructed and original outlines
#   - Error resulting from outline reconstruction as a function of the number of harmonics involved.
# ------------------------------------------------------------------------------

k <- 32

ief <- iefourier(an = ef1$an, bn = ef1$bn, cn = ef1$cn, dn = ef1$dn, k = k, n = 64, ao = ef1$ao, co = ef1$co)

M3 <- cbind(ief$x, ief$y)



# ----------
averagedev <- maxdev <- numeric(k)

for(i in 1:k){
  
  ief <- iefourier(an = ef1$an, bn = ef1$bn, cn = ef1$cn, dn = ef1$dn, k = i, n = 64, ao = ef1$ao, co = ef1$co)
  
  deviation <- sqrt(apply((M3 - cbind(ief$x, ief$y)) ^ 2, 1, sum))

  averagedev[i] <- mean(deviation)
  
  maxdev[i] <- max(deviation)
}



# ----------
par(mfrow=c(1,1))

plot(1:32, maxdev, pch = 4, ylab = "deviation from original outline", xlab = "number of harmonics")

segments(1:32, maxdev, 1:32, averagedev)

lines(1:32, averagedev)

polygon(M_obj, col = "grey", border = NA)

lines(ief1$x, ief1$y, type = "l")



# -->
# Average variation corresponds to the vurve, while maximum deviations are symbolized by crosses



# ------------------------------------------------------------------------------
# Estimate the number of necessary harmonics after examining the spectrum of harmonic Fourier power
#   - Power is proportional to the harmonic amplitude and can be considered as a measure of shape information
# ------------------------------------------------------------------------------

Power <- (ef1$an ^ 2 + ef1$bn ^ 2 + ef1$cn ^ 2 + ef1$dn ^ 2) / 2

(cumsum(Power) / sum(Power))[1:6]



# -->
# Most of the shape "information" is contained in the first harmonic.



# ----------
# if we remove the 1st harmonic
(cumsum(Power[-1]) / sum(Power[-1]))[1:6]



# -->
# In keeping the 2nd to 7th harmonics, more than 99% of the information is remaining.
