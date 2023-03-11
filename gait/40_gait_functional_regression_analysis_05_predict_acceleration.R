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
# Predict knee acceleration from hip acceleration
# ------------------------------------------------------------------------------

D2kneefd     <- deriv(kneefd, 2)

D2hipfd      <- deriv(hipfd, 2)

D2kneemeanfd <- mean(D2kneefd)



# ----------
D2xfdlist  <- list(constfd, D2hipfd)

D2fRegressout <- fRegress(D2kneefd, D2xfdlist, betalist)



# ----------
D2kneehatfd   <- D2fRegressout$yhatfd
D2betaestlist <- D2fRegressout$betaestlist

D2alphafd   <- D2betaestlist[[1]]$fd
D2hipbetafd <- D2betaestlist[[2]]$fd

op <- par(mfrow=c(2,1), ask=FALSE)
plot(D2alphafd,   ylab="D2Intercept")
plot(D2hipbetafd, ylab="D2Hip coefficient")
par(op)



# ----------
# compute and plot squared multiple correlation function

D2kneemat     <- eval.fd(gaitfine, D2kneefd)
#D2kneehatmat  <- eval.fd(gaitfine, D2kneehatfd)
D2kneehatmat  <- predict(D2kneehatfd, gaitfine)
D2kneemeanvec <- as.vector(eval.fd(gaitfine, D2kneemeanfd))

D2SSE0 <- apply((D2kneemat - outer(D2kneemeanvec, rep(1,ncurve)))^2, 1, sum)
D2SSE1 <- apply((D2kneemat - D2kneehatmat)^2, 1, sum)
D2Rsqr <- (D2SSE0 - D2SSE1) / D2SSE0

par(mfrow=c(1,1),ask=FALSE)
plot(gaitfine, D2Rsqr, type="l", ylim=c(0,0.5))



# ----------
# for each case plot the function being fit, the fit, and the mean function
op <- par(mfrow=c(1,1),ask=TRUE)
for (i in 1:ncurve) {
  plot( gaitfine, D2kneemat[,i], type="l", lty=1, col=4, ylim=c(-20,20))
  lines(gaitfine, D2kneemeanvec,           lty=2, col=2)
  lines(gaitfine, D2kneehatmat[,i],        lty=3, col=4)
  lines(c(0,20), c(0,0), lty=2, col=2)
  title(paste("Case",i))
}
par(op)



