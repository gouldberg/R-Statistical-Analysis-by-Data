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
# Compute forcing function from beta coefficients
# ------------------------------------------------------------------------------

# solve equation
result <- odesolv(bwtestlist)
xp <- result[[1]]
yp <- result[[2]]



# ----------
# plot the two solutions
par(mfrow=c(2,1),pty="m")

pltrng <- c(min(yp[1,,]), max(yp[1,,]))
matplot(xp,t(yp[1,,]), type="l", lty=1, ylim=pltrng, main="Function")
abline(h=0, lty=2)

pltrng <- c(min(yp[2,,]), max(yp[2,,]))
matplot(xp,t(yp[2,,]), type="l", lty=1, ylim=pltrng, main="Derivative")
abline(h=0, lty=2)



# ------------------------------------------------------------------------------
# plot fit to each curve
# ------------------------------------------------------------------------------
lipmat   <- eval.fd(liptime, lipfd)
D2lipmat <- eval.fd(liptime, lipfd, 2)

umat <- matrix(0,length(liptime),2)
umat[,1] <- approx(xp, t(yp[1,1,]), liptime)$y
umat[,2] <- approx(xp, t(yp[1,2,]), liptime)$y

par(mfrow=c(1,2),pty="s",ask=TRUE)
index <- 1:20
for (i in index) {
  plot(liptime, force[,i], type="l",
       ylim=c(-1000,1000), xlab="Normalized Time", ylab="", 
       main=paste("Record",i,"Forcing Fn."))
  lines(liptime, D2lipmat[,i],lty=4)
  abline(h=0,lty=2)
  xhat <- lipmat[,i] - lsfit(umat, lipmat[,i], int=FALSE)$residual
  matplot(liptime, cbind(xhat, lipmat[,i]), type="l", lty=c(1,2),
          xlab="Normalized Time", ylab="", main="Function")
}

