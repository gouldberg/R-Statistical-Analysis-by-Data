setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")



# ------------------------------------------------------------------------------
# Squared frequency response functions
#   - Which filters change the input spectrum ?
# ------------------------------------------------------------------------------

# Squared frequency response functions of the first difference
w <- seq(0, 0.5, by = 0.01)


FRdiff <- abs(1 - exp(2i * pi * w))^2



# 12-month moving average filters
u <- cos(2 * pi * w) + cos(4 * pi * w) + cos(6 * pi * w) + cos(8 * pi * w) +  cos(10 * pi * w)


FRma <- ((1 + cos(12 * pi * w) + 2 * u) / 12)^2



# ----------
graphics.off()
par(mfrow = c(2,1))


# Squared frequency response functions
# of the first difference
plot(w, FRdiff, type = "l", xlab = "frequency")

# of 12-month moving average filters
plot(w, FRma, type = "l", xlab = "frequency")



# -->
# First difference filter will attenuate the lower frequencies and enhance the higher frequencies
# because the multiplier of the spectrum is large for the higher frequenceis and small for the lower frequencies.
# Generally the slow rise of this kind of filter does not particularly recommend it as a procedure for retaining only the high freqeuncies

# the centered 12-month moving average filter cut most of the frequency content above 0.05 cycles per point,
# and nearly all of the frequency content above 1/12 = 0.083.
# This drives down the yearly components with periods of 12 months and enhances the El Nino frequency, which is somewhat lower.

# This filter is not completely efficient at attenuating high frequencies; some power contributions are left at higher frequencies
# as shown in first differnce filter's function


