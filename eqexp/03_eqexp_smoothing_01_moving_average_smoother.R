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
# Smoothing in time series context
# Moving average smoother
# ------------------------------------------------------------------------------
# boxcar-type weights

l <- 20
( wgts <- c(.5, rep(1, l-1), .5) / l )
plot(wgts, type = "b")


obj_ts <- EQ5

# sides = 2:  for convolution filters only, if sides = 1 the filter coeffs are for past values only, if side = 2 they are centered around lag 0
( objf <- stats::filter(obj_ts, sides = 2, filter = wgts) )



# ----------
par(mfrow=c(1,1))
plot(obj_ts, type = "l")
lines(objf, lwd=2, col = 4)



# -->
# the moving average smoother produces somewhat choppy line.



# ----------
nwgts <- c(rep(0, 20), wgts, rep(0, 20))
plot(nwgts, type = "l", ylim = c(-0.02, 0.1), xaxt = 'n', yaxt = 'n', ann = FALSE)

