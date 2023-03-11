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
# Spectral analysis:  Coherence between SOI and Recruitment
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
#        An important example of the application of the cross-spectrum is to the problem of predicting an output series y(t) from
#        some input series x(t) through a linear filter relation such as the three-point moving average.
#        A measure of the strength of such a relation is the squared coherence.
# ------------------------------------------------------------------------------

# here we try L = 19

L <- 19

( m <- (L - 1) / 2 )


sr <- mvspec(cbind(soi, rec), kernel("daniell", m), plot = FALSE)



# ----------
sr$df



# ----------
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(2,1))

plot(sr)

plot(sr, plot.type = "coh", ci.lty = 2)

abline(h = C)




# -->
# We may reject the hypothesis of no coherence for values of rho(y*x)^2(omega) that exceed C(0.001) = 0.32

# The soi and rec are obviously strongly coherent at the annual seasonal frequency
# The series are also strongly coherent at lower frequencies that may be attributed to the El Nino cycle,
# which we claimed had a 3 to 7 year period.

# The peak in the coherency, however, occurs closer to the 9 year cycle.
# Other frequencies are also coherent, although the strong coherence is less impressive
# because the underlying power spectrum at these higher frequencies is fairly small.
# We note that the coherence is persistent at the seasonal harmonic frequencies.

