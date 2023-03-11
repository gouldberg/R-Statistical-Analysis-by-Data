setwd("//media//kswada//MyFiles//R//birth")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birth
# ------------------------------------------------------------------------------

data(birth, package = "astsa")

str(birth)

head(birth)



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

L <- 9
L <- 3

m <- (L-1)/2


par(mfrow=c(2,1))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
bir.ave <- astsa::mvspec(birth, kernel("daniell", m), log = "no")


# ----------
bir.ave <- astsa::mvspec(diff(log(birth)), kernel("daniell", m), log = "no")
