setwd("//media//kswada//MyFiles//R//so2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  so2
# ------------------------------------------------------------------------------

data(so2, package = "astsa")

str(so2)

so2



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

L <- 9
# L <- 3

m <- (L-1)/2


par(mfrow=c(2,1))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1

so2.ave <- astsa::mvspec(so2, kernel("daniell", m), log = "no")

so2.ave <- astsa::mvspec(diff(so2), kernel("daniell", m), log = "no")




# -->
# averaged periodgram is better describing cycles than raw periodgram
s

