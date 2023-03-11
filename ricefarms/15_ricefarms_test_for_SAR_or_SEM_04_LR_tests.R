setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# LR tests for SEM or SAR
# ------------------------------------------------------------------------------


# The restriction test for the SAR term is perfomed as

ll1 <- saremremod$logLik


ll0 <- spml(riceprod, data = Rice, listw = ricelw, lag = FALSE, model ="random", spatial.error = "b")$logLik



# ----------
LR <- 2 * (ll1 - ll0)


pLR <- pchisq(LR, df = 1, lower.tail = FALSE)


pLR



# -->
# the p-value from the LR spatial log test is very high,
# and not unlike the (asymptoticaly equivalent) result from the Wald restriction test


