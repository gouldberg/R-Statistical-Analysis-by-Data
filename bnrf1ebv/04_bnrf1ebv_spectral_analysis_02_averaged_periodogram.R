setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

# L <- 9
L <- 21

m <- (L-1)/2


par(mfrow=c(1,1))

# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
bnr.ave <- astsa::mvspec(bnrf1ebv, kernel("daniell", m), log = "no")



# ------------------------------------------------------------------------------
# bandwidth and degrees of freedom
# ------------------------------------------------------------------------------

bnr.ave$bandwidth


bnr.ave$df

