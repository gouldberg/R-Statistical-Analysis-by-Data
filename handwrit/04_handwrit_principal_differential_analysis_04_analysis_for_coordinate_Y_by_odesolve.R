setwd("//media//kswada//MyFiles//R//handwrit")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  handwrit
#   - handwriting of "fda" of 20 smaples
# ------------------------------------------------------------------------------
data("handwrit", package = "fda")

str(handwrit)



# ------------------------------------------------------------------------------
# Solve by odesolv()
# ------------------------------------------------------------------------------
# Define a functional data object for the three derivative weight functions

wcoef1 <- pdaListY2$bwtlist[[1]]$fd$coefs
wcoef2 <- pdaListY2$bwtlist[[2]]$fd$coefs
wcoef3 <- pdaListY2$bwtlist[[3]]$fd$coefs

wcoef  <- cbind(wcoef1, wcoef2, wcoef3)

wfd    <- fd(wcoef, wbasis125)



# ----------
# Set up a linear differential operator.
# This isn't used in these analyses.

fdaLfd <- Lfd(difeorder, fd2list(wfd))

ystart <- matrix(0, 3, 3)
ystart[1,1] <- fdameanmat[1,2]
ystart[2,2] <- D1fdameanmat[1,2]
ystart[3,3] <- D2fdameanmat[1,2]



# ----------
# solve the equation 

EPSval = 1e-4
odeList <- odesolv(pdaList2$bwtlist, ystart, EPS=EPSval, MAXSTP=1e6)




# ------------------------------------------------------------------------------
# plot the three solutions
# ------------------------------------------------------------------------------

tY <- odeList[[1]]
yY <- odeList[[2]]



# ----------
par(mfrow=c(3,1), pty="m")

pltrng <- c(min(yY[1,,]), max(yY[1,,]))
matplot(tY, t(yY[1,,]), type="l", lty=1, ylim=pltrng, main="Function")
abline(h=0, lty=2)

pltrng <- c(min(yY[2,,]), max(yY[2,,]))
matplot(tY, t(yY[2,,]), type="l", lty=1, ylim=pltrng, main="Derivative")
abline(h=0, lty=2)

pltrng <- c(min(yY[3,,]), max(yY[3,,]))
matplot(tY, t(yY[3,,]), type="l", lty=1, ylim=pltrng, main="Derivative")
abline(h=0, lty=2)



# ------------------------------------------------------------------------------
# plot fit to each curve
# ------------------------------------------------------------------------------

# set up curve and derivative values
umaty <- matrix(0, length(fdatime), 3)
umaty[,1] <- approx(tY, t(yY[1,1,]), fdatime)$y
umaty[,2] <- approx(tY, t(yY[1,2,]), fdatime)$y
umaty[,3] <- approx(tY, t(yY[1,3,]), fdatime)$y

Dumaty <- matrix(0, length(fdatime), 3)
Dumaty[,1] <- approx(tY, t(yY[2,1,]), fdatime)$y
Dumaty[,2] <- approx(tY, t(yY[2,2,]), fdatime)$y
Dumaty[,3] <- approx(tY, t(yY[2,3,]), fdatime)$y

D2umaty <- matrix(0, length(fdatime), 3)
D2umaty[,1] <- approx(tY, t(yY[3,1,]), fdatime)$y
D2umaty[,2] <- approx(tY, t(yY[3,2,]), fdatime)$y
D2umaty[,3] <- approx(tY, t(yY[3,3,]), fdatime)$y



# ----------
# plot fit to each curve
par(mfrow=c(1,1), ask=TRUE, pty="m")

index  <- 1:20

fdamat <- array(0, c(1401, 20, 2))

fdamat[,,1] <- eval.fd(fdatime, fdafdX)

fdamat[,,2] <- eval.fd(fdatime, fdafdY)

zmat   <- cbind(fdatime-1150, umaty)

for (i in index) {
  yhat <- fdamat[,i,2] - lsfit(zmat, fdamat[,i,2])$residual
  
  matplot(fdatime, cbind(yhat, fdamat[,i,2]), 
          type="l", lty=c(1,3), cex=1.2, 
          xlim=c(0, 2300), ylim=c(-0.04, 0.04),   
          main=paste("Y curve ",i))
}

