setwd("//media//kswada//MyFiles//R//sunspotz")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sunspotz
# ------------------------------------------------------------------------------

data(sunspotz, package = "astsa")

str(sunspotz)

head(sunspotz)



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

# Trying values of small L
L <- 3

m <- (L-1)/2


par(mfrow=c(1,1))

# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
sun.ave <- astsa::mvspec(sunspotz, kernel("daniell", m), log = "no")


