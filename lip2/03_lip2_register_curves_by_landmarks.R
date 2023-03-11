setwd("//media//kswada//MyFiles//R//lip2")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lip2
#   - The data of the movement of lips during speech production
# ------------------------------------------------------------------------------

data(lip, package = "fda")

dim(lip)


head(lip)


# -----------
liptime

lipmarks



# ------------------------------------------------------------------------------
# Identify landmarks
# ------------------------------------------------------------------------------

# takes mean of leftElbow and rightElbow
lipmeanmarks <- apply(lipmarks, 2, mean)



# ------------------------------------------------------------------------------
# Optionally:  Identify points as landmarks in each curve by manually 
# ------------------------------------------------------------------------------

nmarks = 2

lipmarks2 <- matrix(0, 20, nmarks)

index <- 1:20


par(mfrow=c(1,1), pty="m")

for (i in index) {
  plot(liptime, lip[,i], xlab="", ylab="", main=paste("Curve",i))
  indexi <- identify(liptime, lip[,i], n = nmarks)
  lipmarks2[i,] <- liptime[indexi]
}



# ------------------------------------------------------------------------------
# Register by landmarks
# ------------------------------------------------------------------------------

# First create a basis object for the warping function.
# it has order 4 (piecewise cubic) and two interior knots positioned at the mean landmark values
# since NBASIS = NORDER + # interior knots

wnbasis <- 6

wnorder <- 4

wbreaks <- c(0, lipmeanmarks, 0.35) 


# warpbasis <- create.bspline.basis(liprange, wnbasis, wnorder, wbreaks)
# warpbasis <- create.bspline.basis(range(lip), wnbasis, wnorder, wbreaks)
warpbasis <- create.bspline.basis(nbasis = wnbasis, norder = wnorder, breaks = wbreaks)

fd(basisobj = warpbasis)

WfdPar    <- fdPar(fd(basisobj = warpbasis), 2, 1e-4)

# WfdPar.    <- fdPar(fd(matrix(0, wnbasis, 1), warpbasis), 2, 1e-4)
# all.equal(WfdPar, WfdPar.)



# ----------
lambda <- 1e-12

lipfd <- smooth.basisPar(liptime, lip, 6, Lfdobj=int2Lfd(4), lambda = lambda)$fd

lipreglist <- landmarkreg(lipfd, as.matrix(lipmarks), lipmeanmarks, WfdPar)


lipregfd   <- lipreglist$regfd

lipwarpfd  <- lipreglist$warpfd



# ------------------------------------------------------------------------------
# plot unregistered and registered curves
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2), pty="s")

plot(lipfd, main = "Unregistered")
# lines(lipmeanfd, lty = 2)
abline(v = lipmeanmarks, lty = 2)

plot(lipregfd, main="Registered")
# lines(lipmeanfd, lty = 2)
abline(v = lipmeanmarks, lty = 2)



# ------------------------------------------------------------------------------
# plot warping functions and deformations
# ------------------------------------------------------------------------------

par(mfrow=c(1,2), pty="s")

# warping function
plot(lipwarpfd, href=FALSE, main="Warping Functions")
abline(0, 1, lty=2)


# warping deformation function
hmat <- eval.fd(liptime, lipwarpfd)
defmat <- hmat - outer(liptime, rep(1,20))

matplot(liptime, defmat, type="l", lty=1, 
        xlab="Normalized time",  ylab="Warped Normalized time",
        main="Deformation Functions")
abline(h = 0, lty = 2)

