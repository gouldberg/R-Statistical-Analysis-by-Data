setwd("//media//kswada//MyFiles//R//pinch")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pinch
# ------------------------------------------------------------------------------

pinch <- matrix(scan("pinch.txt", 0), 151, 20, byrow=TRUE)

# data("pinch", package = "fda")

head(pinch)



# ------------------------------------------------------------------------------
# Plot individual velocity and acceleartion
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,1))

plot(deriv(pinchfd, 1), xlab='', ylab='', main='Force Velocity', lwd=2)

plot(deriv(pinchfd, 2), xlab='', ylab='', main='Force Acceleration', lwd=2)
