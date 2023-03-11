setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  seabird
# ------------------------------------------------------------------------------
data("seabird", package = "fda")

str(seabird)

car::some(seabird)



# ------------------------------------------------------------------------------
# Confidence intervals for regression functions
# ------------------------------------------------------------------------------

# error variace
birdYhatmat = eval.fd(yearCode, birdYhatfdobj$fd[1:26])

rmatb   = logCounts2 - birdYhatmat

SigmaEb = var(t(rmatb))



# ----------
# compute y2cMap:  used to compute the regression coefficient matrix from the original observations
y2cMap.bird = birdlist2$y2cMap



# ----------
# standard errors
birdStderrList = fRegress.stderr(fitShellfish.5, y2cMap.bird, SigmaEb)

birdBeta.sdList = birdStderrList$betastderrlist



# ----------
# plot by plotbeta(): plot a functional parameter object with confidence limits
op = par(mfrow=c(2,1))
plotbeta(birdBetaestlist[1:2], birdBeta.sdList[1:2])
par(op)





