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
# Compute and plot the acceleration magnitudes:  tangential accelerations
# ------------------------------------------------------------------------------
D2mag  <- sqrt(D2fdamat[,,1]^2 + D2fdamat[,,2]^2)

D2magmean <- apply(D2mag, 1, mean)


par(mfrow=c(1,1), ask=FALSE)

matplot(fdatime, 1e6 * D2mag, type = "l", cex = 1.2,
        xlab="Milliseconds", ylab="Metres/sec/sec",
        xlim=c(0, 2300), ylim=c(0, 12),
        main="Acceleration Magnitude")

plotrange <- c(0,12)

for (k in 1:length(cycle)) abline(v=cycle[k], lty=2)

for (j in 1:length(featureindex)) abline(v=cycle[featureindex[j]], lty=1)



# ----------
# By each curve
# Note the two rest cycles, one in "d" and one in "a"

par(mfrow=c(1,1))

plot(fdatime, 1e6 * D2magmean, type = "l", cex = 1.2,
     xlab="Milliseconds", ylab="Metres/sec/sec",
     xlim=c(0,2300), ylim=c(0,8),
     main="Mean acceleration Magnitude")

plotrange <- c(0,8)

for (k in 1:length(cycle)) abline(v=cycle[k], lty=2)

for (j in 1:length(featureindex)) abline(v=cycle[featureindex[j]], lty=1)



# ------------------------------------------------------------------------------
# Plot each individual acceleration magnitude, along with the mean magnitude as a green dashed line
# ------------------------------------------------------------------------------

par(mfrow=c(1,1), ask=TRUE)

plotrange <- c(0,12)

for (i in 1:20) {
  plot(fdatime, 1e6*D2mag[,i], type="l", cex=1.2, 
       xlim=c(0,2300), ylim=c(0,12), 
       xlab="Milliseconds", ylab="Metres/sec/sec",
       main=paste("Script ",i))
  lines(fdatime,  1e6*D2magmean, lty=3)

  for (k in 1:length(cycle)) abline(v=cycle[k], lty=2)
  
  for (j in 1:length(featureindex)) abline(v=cycle[featureindex[j]], lty=1)
}


