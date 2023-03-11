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
# Concurrent model:  set up
#   - dependent variable:  tray 47 level
#   - independent variable:  reflux flow
#
#   - Concurrent model relates the value of y(i)(t) to the value of x(ij)(t) at the same time points 5.
#   - The intercept function bet0(t) in effect multiplies a scalar covariate whose value is always one,
#     and captures the variation in the response that does not depend on any of the covariate functions.
# ------------------------------------------------------------------------------

( bbreaks   <- c(seq(0, tbreak, len=2), tbreak, seq(tbreak, refrange[2], len=5)) )

norder    <- 4
nbbasis   <- length(bbreaks) + norder - 2

bbasis    <- create.bspline.basis(refrange, nbbasis, norder, bbreaks)

p <- 1
betafd    <- fd(matrix(0, nbbasis, p), bbasis)

betafdPar <- fdPar(betafd, 2, 1e-4)

betalist <- list(betafdPar)




# ----------
# set up basis for input variable

norder  <- 1
nubasis <- 2

ubreaks <- c(refrange[1], tbreak, refrange[2])

ubasis  <- create.bspline.basis(refrange, nubasis, norder, ubreaks)

ufd <- smooth.basis(tval, uval, ubasis)$fd

xfdlist  <- list(ufd)



# ------------------------------------------------------------------------------
# Concurrent model analysis
#   - dependent variable:  tray 47 level
#   - independent variable:  reflux flow
# ------------------------------------------------------------------------------

fRegressList <- fRegress(yfd, xfdlist, betalist)

betaestlist <- fRegressList$betaestlist

yhatfdobj   <- fRegressList$yhatfdobj



# ----------
# plot the regression function

betafdPar <- betaestlist[[1]]

betafd    <- betafdPar$fd

plot(betafd, xlab="minutes", ylab="beta(t)")



# ----------
# plot the data and the fit
yhatvec <- predict(yhatfdobj, tval)

par(mfrow=c(1,1))
plot(tval, yval, type="p", xlim=refrange, xlab="minutes", ylab="Tray 47 level")
lines(tval, yhatvec, lwd=3)

