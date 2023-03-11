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
# compute values of smoothed and interpolated curves
# ------------------------------------------------------------------------------

fdameanfd  <- mean(fdafd)

fdamat     <- eval.fd(fdatime, fdafd)

fdameanmat <- apply(fdamat, c(1,3), mean)



# ------------------------------------------------------------------------------
# Evaluate curves at cycle times
#   - Show the points of motor control clock cycle times at every 119 milliseconds
# ------------------------------------------------------------------------------
cycle  <- seq(0, 2300, 119)

ncycle <- length(cycle)



# ----------
# evaluate curves at cycle times
( fdamatcycle     <- eval.fd(cycle, fdafd) )

( fdameanmatcycle <- apply(fdamatcycle, c(1,3), mean) )



# ----------
#  Indices of cycle times corresponding to important features:
#  -- the cusp in "f", 
#  -- the the cusp in "d", 
#  -- the first cusp in "a", 
#  -- the rest after the first cusp in "a", and 
#  -- the second cusp in "a".
#  It is remarkable that these features correspond so closely
#  with clock cycle times!

featureindex   <- c(3, 5, 7, 10, 13, 16, 19)

fdafeature     <- fdamatcycle[featureindex,,]

fdameanfeature <- fdameanmatcycle[featureindex,]



# ----------
# Plot mean, including both sampling points and fit points at cycle times are plotted as blue circles, and
# points at feature times are plotted as red circles.

par(mfrow=c(1,1))

plot(fdameanmat[,1], fdameanmat[,2], type="l", lwd=2,
     xlab="Metres", ylab="Metres", 
     xlim=c(-.040, .040), ylim=c(-.040, .040),
     main="Mean script")

points(fdameanmatcycle[-featureindex,1], fdameanmatcycle[-featureindex,2], cex=1.2, col=2, lwd=4)

points(fdameanfeature[,1], fdameanfeature[,2], cex=1.2, col=3, lwd=4)



# ------------------------------------------------------------------------------
# Evaluate curves at cycle times:  plot individual curves
#   - Plot individual curves, including both sampling points and fit also plot the mean curve in the background.
#   - Note how closely individual curve features are tied to the feature cycle times.
# ------------------------------------------------------------------------------

par(mfrow=c(1,1), ask=TRUE)

for (i in 1:20) {
  plot(fdamat[,i,1], fdamat[,i,2], type="l", lwd=2,  
       xlab="Metres", ylab="Metres",
       xlim=c(-.040, .040), ylim=c(-.040, .040),
       main=paste("Script",i))
  points(fdamatcycle[-featureindex,i,1],   
         fdamatcycle[-featureindex,i,2],         cex=1.2,col=2,lwd=4) 
  points(fdafeature[,i,1],    fdafeature[,i,2],  cex=1.2,col=3,lwd=4) 
  lines( fdameanmat[,1],      fdameanmat[,2],      lty=4) 
  points(fdameanmatcycle[-featureindex,1], 
         fdameanmatcycle[-featureindex,2],         cex=1.2,col=2,lwd=4) 
  points(fdameanfeature[,1],  fdameanfeature[,2],  cex=1.2,col=3,lwd=4)
}

