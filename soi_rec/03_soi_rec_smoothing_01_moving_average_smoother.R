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
# Smoothing in time series context
# Moving average smoother
# ------------------------------------------------------------------------------

# boxcar-type weights
( wgts <- c(.5, rep(1,11), .5) / 12 )

plot(wgts, type = "b")


# sides = 2:  for convolution filters only, if sides = 1 the filter coeffs are for past values only, if side = 2 they are centered around lag 0
( soif <- stats::filter(soi, sides = 2, filter = wgts) )



# ----------
par(mfrow=c(1,1))
plot(soi)
lines(soif, lwd=2, col = 4)



# -->
# Although the moving average smoother does a good job in highlighting the El Nino effect, it might be considered too choppy.



# ----------
nwgts <- c(rep(0, 20), wgts, rep(0, 20))
plot(nwgts, type = "l", ylim = c(-0.02, 0.1), xaxt = 'n', yaxt = 'n', ann = FALSE)

