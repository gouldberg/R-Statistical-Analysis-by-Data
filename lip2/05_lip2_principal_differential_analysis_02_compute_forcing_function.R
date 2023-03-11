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



# ------------------------------------------------------------------------------
# Compute forcing functions
# ------------------------------------------------------------------------------

# compute forcing functions
Lfdest <- Lfd(2, bwtestlist)

force        <- eval.fd(liptime, lipfd, Lfdest)
lipaccel     <- eval.fd(liptime, lipfd, 2)
lipmeanaccel <- apply(lipaccel, 1, mean)



# ------------------------------------------------------------------------------
# Plot the mean forcing function
# ------------------------------------------------------------------------------
par(mfrow=c(1,1), ask=FALSE)
yrange <- c(min(min(lipmeanaccel), min(force)), max(max(lipmeanaccel), max(force)))
matplot(liptime, force, type="l", lty=1, ylim=yrange)
lines(liptime, lipmeanaccel, lty=4, lwd=2)



# ----------
# plot the mean forcing function along with second deriv.

forcemean <- apply(force, 1, mean)
plot(liptime, forcemean, type="l", lty=1, ylim=yrange)
lines(liptime, lipmeanaccel, lty=4)
