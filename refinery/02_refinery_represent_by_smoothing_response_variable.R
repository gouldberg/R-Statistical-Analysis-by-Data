setwd("//media//kswada//MyFiles//R//refinery")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  refinery
#   - Data collected at an oil refinery in Texas.
# ------------------------------------------------------------------------------
data(refinery, package = "fda")

str(refinery)


head(refinery)



# ------------------------------------------------------------------------------
# Centering values
# ------------------------------------------------------------------------------

# observation time
tval <- refinery[,1]

# reflux flow
uval <- refinery[,2]

# tray 47 level
yval <- refinery[,3]

n <- length(tval)


# ----------
# center the data on mean values prior to change
uval <- uval - mean(uval[1:60])

yval <- yval - mean(yval[1:60])


# ----------
op <- par(mfrow=c(2,1), pty="m")
plot(tval, yval, type="p", ylim=c(-0.5,4.5), xlab="Time", ylab="Tray 47 level")
plot(tval, uval, type="p", ylim=c(-0.6,0.2), xlab="Time", ylab="Reflux flow")
par(op)



# ------------------------------------------------------------------------------
# Smoothing response varaiable with knots
# ------------------------------------------------------------------------------

refrange <- c(tval[1], tval[n])

tbreak   <- tval[67]



# ----------
# set up basis for the output variable
# put three coincident knots at tbreak

norder  <-  4

( yknots  <- c(seq(0, tbreak, len=3), tbreak, seq(tbreak, refrange[2], len=5)) )

nybasis <- length(yknots) + norder - 2

ybasis  <- create.bspline.basis(refrange, nybasis, norder, yknots)

yfd <- smooth.basis(tval, yval, ybasis)$fd

yvec <- eval.fd(tval, yfd)



# ----------
# plot the data with fits and knots

par(mfrow=c(1,1))
plot(tval, yval, type="p", xlab="minutes", ylab="Tray 47 level", xlim=refrange)
lines(tval, yvec, lwd=3)
for (i in 2:length(yknots)-1) lines(c(yknots[i], yknots[i]), c(0,4), lty=4, lwd=2)


