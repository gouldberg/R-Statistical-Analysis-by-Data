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
# Evaluate three derivatives and their means
# ------------------------------------------------------------------------------

D1fdamat <- eval.fd(fdatime, fdafd, 1)

D2fdamat <- eval.fd(fdatime, fdafd, 2)

D3fdamat <- eval.fd(fdatime, fdafd, 3)

D1fdameanmat <- apply(D1fdamat, c(1,3), mean)

D2fdameanmat <- apply(D2fdamat, c(1,3), mean)

D3fdameanmat <- apply(D3fdamat, c(1,3), mean)



# ----------
# Plot the individual acceleration records.
# In these plots, acceleration is displayed as metres per second per second.

# Cycle and feature times are plotted as vertical dashed lines, un-featured cycle times as red 
# dotted lines, and cycle times of features as heavier magenta solid lines.

par(mfrow=c(1,1), mar=c(5,5,4,2), ask=TRUE)

for (i in 1:20) {
  matplot(fdatime, cbind(1e6 * D2fdamat[,i,1], 1e6 * D2fdamat[,i,2]), 
          type="l", lty=1, cex=1.2, col=c(2,4),
          xlim=c(0, 2300), ylim=c(-12, 12),
          xlab="Milliseconds", ylab="Meters/msec/msec",
          main=paste("Curve ",i))
  abline(h=0, lty=2)
  plotrange <- c(-12, 12)
  for (k in 1:length(cycle)) abline(v=cycle[k], lty=2)
  for (j in 1:length(featureindex)) 
    abline(v=cycle[featureindex[j]], lty=1)
  legend(1800,11.5, c("X", "Y"), lty=1, col=c(2,4))
}

