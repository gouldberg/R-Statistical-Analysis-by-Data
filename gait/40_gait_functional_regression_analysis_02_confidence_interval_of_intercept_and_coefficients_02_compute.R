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
# Plot intercept and hip regression coefficients with 95% confidence intervals
# ------------------------------------------------------------------------------

gaitbasismat = eval.basis(gaitfine, gaitbasis)

y2cMap = gaitSmooth$y2cMap


fRegressList1 = fRegress(kneefd, xfdlist, betalist, y2cMap=y2cMap, SigmaE=SigmaE)

fRegressList2 = fRegress.stderr(fRegressList1, y2cMap, SigmaE)

betastderrlist = fRegressList2$betastderrlist

op = par(mfrow=c(2,1), mar=c(2,2,2,2))
plotbeta(betaestlist, betastderrlist, gaitfine)
par(op)



# -->
# We see that hip angle variation is coupled to knee angle variation in the middle of each of these three episodes,
# and the relation is especially strong during the middle flexing phase.
# It seems logical that a strongly flexed knee is associated with a shaper hip angle.


