setwd("//media//kswada//MyFiles//R//refinery")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  refinery
#   - Data collected at an oil refinery in Texas.
# ------------------------------------------------------------------------------
data(refinery, package = "fda")

str(refinery)


head(refinery)



# ------------------------------------------------------------------------------
# Concurrent model analysis:  set up
#   - dependent variable:  derivative of tray 47 level
#   - independent variable:  tray 47 level and reflux flow
#
#   - Concurrent model relates the value of y(i)(t) to the value of x(ij)(t) at the same time points 5.
#   - The intercept function bet0(t) in effect multiplies a scalar covariate whose value is always one,
#     and captures the variation in the response that does not depend on any of the covariate functions.
# ------------------------------------------------------------------------------

nDybasis <- length(yknots) + 3 - 2

Dybasis  <- create.bspline.basis(refrange, nDybasis, 3, yknots)

( tvalD <- c(tval[1:66], tbreak - 1e-5, tbreak + 1e-5, tval[68:n]) )

Dyvec <- eval.fd(tvalD, yfd, 1)

Dyfd  <- smooth.basis(tvalD, Dyvec, Dybasis)$fd

Dyfdhat <- eval.fd(tvalD, Dyfd)



# p <- 2
bbasis    <- create.constant.basis(refrange)

betafd    <- fd(0, bbasis)

betafdPar <- fdPar(betafd)

betalist[[1]] <- betafdPar
betalist[[2]] <- betafdPar

xfdlist[[1]] <- yfd
xfdlist[[2]] <- ufd



# ----------
# carry out the analysis

fRegressList <- fRegress(Dyfd, xfdlist, betalist)

betaestlist <- fRegressList$betaestlist
Dyhatfdobj  <- fRegressList$yhatfdobj


betafdPar1 <- betaestlist[[1]]
betafdPar2 <- betaestlist[[2]]

beta1 <- betafdPar1$fd$coefs
beta2 <- betafdPar2$fd$coefs

print(paste("Beta_1 =",round(beta1,3)))
print(paste("Beta_2 =",round(beta2,3)))



# ----------
# plot the first derivative and fit

# Dyhatvec <- eval.fd(tvalD, Dyhatfdobj)
Dyhatvec <- predict(Dyhatfdobj, tvalD)

plot(Dyfd, ylim=c(-.02,0.1))
lines(tvalD, Dyhatvec, lwd=3)



# ----------
# plot the data and the fit

nD <- length(tvalD)

yhatvec <- rep(0,nD)

yhatvec[68:nD] <- 0.4924 * (beta2 / beta1) * (1 - exp(beta1 * (tval[68:nD] - 67)))

par(mfrow=c(1,1))
plot(tval, yval, type="p", xlim=refrange, xlab="minutes", ylab="Tray 47 level")
lines(tvalD, yhatvec, lwd=3)
