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

wcoef1 <- pdaList2$bwtlist[[1]]$fd$coefs
wcoef2 <- pdaList2$bwtlist[[2]]$fd$coefs
wcoef3 <- pdaList2$bwtlist[[3]]$fd$coefs

wcoef  <- cbind(wcoef1, wcoef2, wcoef3)

wfd    <- fd(wcoef, wbasis125)



# ----------
# Set up a linear differential operator.
# This isn't used in these analyses.

fdaLfd <- Lfd(difeorder, fd2list(wfd))

ystart <- matrix(0,3,3)
ystart[1,1] <- fdameanmat[1,1]
ystart[2,2] <- D1fdameanmat[1,1]
ystart[3,3] <- D2fdameanmat[1,1]



# ----------
# solve the equation 

EPSval = 1e-4
odeList <- odesolv(pdaList2$bwtlist, ystart, EPS=EPSval, MAXSTP=1e6)




# ------------------------------------------------------------------------------
# plot the three solutions
# ------------------------------------------------------------------------------

tX <- odeList[[1]]
yX <- odeList[[2]]



# ----------
par(mfrow=c(3,1), pty="m")

pltrng <- c(min(yX[1,,]), max(yX[1,,]))
matplot(tX, t(yX[1,,]), type="l", lty=1, ylim=pltrng, main="Function")
abline(h=0, lty=2)

pltrng <- c(min(yX[2,,]), max(yX[2,,]))
matplot(tX, t(yX[2,,]), type="l", lty=1, ylim=pltrng, main="Derivative")
abline(h=0, lty=2)

pltrng <- c(min(yX[3,,]), max(yX[3,,]))
matplot(tX, t(yX[3,,]), type="l", lty=1, ylim=pltrng, main="Derivative")
abline(h=0, lty=2)



# ------------------------------------------------------------------------------
# plot fit to each curve
# ------------------------------------------------------------------------------

# set up curve and derivative values
umatx <- matrix(0, length(fdatime), 3)
umatx[,1] <- approx(tX, t(yX[1, 1,]), fdatime)$y
umatx[,2] <- approx(tX, t(yX[1, 2,]), fdatime)$y
umatx[,3] <- approx(tX, t(yX[1, 3,]), fdatime)$y

Dumatx <- matrix(0, length(fdatime), 3)
Dumatx[,1] <- approx(tX, t(yX[2, 1,]), fdatime)$y
Dumatx[,2] <- approx(tX, t(yX[2, 2,]), fdatime)$y
Dumatx[,3] <- approx(tX, t(yX[2, 3,]), fdatime)$y

D2umatx <- matrix(0, length(fdatime), 3)
D2umatx[,1] <- approx(tX, t(yX[3, 1,]), fdatime)$y
D2umatx[,2] <- approx(tX, t(yX[3, 2,]), fdatime)$y
D2umatx[,3] <- approx(tX, t(yX[3, 3,]), fdatime)$y



# ----------
# plot fit to each curve
par(mfrow=c(1,1), ask=TRUE, pty="m")

index  <- 1:20

fdamat <- array(0, c(1401, 20, 1))

fdamat[,,1] <- eval.fd(fdatime, fdafdX)

zmat   <- cbind(fdatime - 1150, umatx)

for (i in index) {
  xhat <- fdamat[,i,1] - lsfit(zmat, fdamat[,i,1])$residual
  
  matplot(fdatime, cbind(xhat, fdamat[,i,1]), 
          type="l", lty=c(1,3), cex=1.2,
          xlim=c(0, 2300), ylim=c(-0.04, 0.04),   
          main=paste("X curve ", i))
}


