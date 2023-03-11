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
# Plot all smoothed and interpolated curves
# ------------------------------------------------------------------------------

par(mfrow=c(2,1), mar=c(2,2,2,2))

plot(gaitfd, cex=1.2)




# ------------------------------------------------------------------------------
# Plot each pair of curves interactively
# ------------------------------------------------------------------------------

op <- par(mfrow=c(2,1))
plotfit.fd(gait, gaittime, gaitfd, cex=1.2)
par(op)


graphics.off()



# ------------------------------------------------------------------------------
# Plot residuals, sorting cases by residual sum of squares
# ------------------------------------------------------------------------------
plotfit.fd(gait, gaittime, gaitfd, residual=TRUE, sort=TRUE, cex=1.2)



# ------------------------------------------------------------------------------
# Plot first derivative of all curves
# ------------------------------------------------------------------------------

plot(gaitfd, Lfdob=1)



# ------------------------------------------------------------------------------
# Plot second derivatives of all curves
# ------------------------------------------------------------------------------

plot(gaitfd, Lfdob=2)
