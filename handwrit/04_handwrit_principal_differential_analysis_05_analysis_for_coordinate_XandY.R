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
# plot fit
# ------------------------------------------------------------------------------

# plot fit to each script
# hit any key after each plot

par(mfrow=c(1,1), ask=TRUE, pty="m")

index   <- 1:20

zmatx   <- cbind(matrix(1, 1401, 1), umatx)

zmaty   <- cbind(matrix(1, 1401, 1), umaty)

for (i in index) {
  xhat <- fdamat[,i,1] - lsfit(umatx, fdamat[,i,1])$residual
  
  yhat <- fdamat[,i,2] - lsfit(umaty, fdamat[,i,2])$residual
  
  plot(xhat, yhat, type="l",
       xlim=c(-0.05, 0.05), ylim=c(-0.05, 0.05),
       xlab="X", ylab="Y", main=paste("Script ",i))

  lines(fdamat[,i,1], fdamat[,i,2], lty=3)
}




# ------------------------------------------------------------------------------
# plot the two weight functions for the second derivative
# ------------------------------------------------------------------------------
par(mfrow=c(1,1), mar=c(5,5,4,2), pty="m")

matplot(fdatime, cbind(b2vecX, b2vecY), type="l", lty=1, col=c(2,4), xlim=c(0, 2300), ylim=c(0, 6e-3))

abline(h=b2meanX, lty=3, col=2)

abline(h=b2meanY, lty=3, col=4)

plotrange <- c(0,6e-3)
for (k in 1:length(cycle)) abline(v=cycle[k], lty=2)
for (j in 1:length(featureindex)) abline(v=cycle[featureindex[j]], lty=1)




# -->
# Conclusions:

# For each coordinate, a single differential equation was estimated to describe all 20 coordinate records simulataneously.
# The differential equation solutions are readable.
# Moreover, they accommodate a good deal of the curve-to-curve variation in the scripts.  
# The cusp first in the "a" is not handled as well as the other cusps.
# This may be due to poor registration at this point, or to the fact that this cusp has a lot of variation from one replication to another.

