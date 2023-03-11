setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)



# ------------------------------------------------------------------------------
# Plot warping functions
# ------------------------------------------------------------------------------

op <- par(mfrow=c(1,1), mar=c(4,4,2,1), pty="m")
matplot(xfine, warpmat, type="l", xlab="t", ylab="h(t)", main="Time warping functions", cex=1.2)
par(op)




# ------------------------------------------------------------------------------
# Plot the deformation functions, def(t) = warp(t) - t
# ------------------------------------------------------------------------------

defmat <- warpmat
for (i in 1:39) defmat[,i] <- warpmat[,i] - xfine - shift[i]

op <- par(mfrow=c(1,1), mar=c(4,4,2,1), pty="m")
matplot(xfine, defmat, type="l", xlab="t", ylab="h(t) - t", main="Deformation functions", cex=1.2)
par(op)

