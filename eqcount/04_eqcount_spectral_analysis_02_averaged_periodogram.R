setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")

str(EQcount)

EQcount



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

L <- 1
L <- 3

m <- (L-1)/2


par(mfrow=c(2,1))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
eqc.ave <- astsa::mvspec(EQcount, kernel("daniell", m), log = "no")

