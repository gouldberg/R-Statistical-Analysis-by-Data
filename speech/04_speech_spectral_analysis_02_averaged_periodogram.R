setwd("//media//kswada//MyFiles//R//speech")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  speech
# ------------------------------------------------------------------------------

data(speech, package = "astsa")

str(speech)


head(speech)



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

# Trying values of L leads to the choice L = 9 as a reasonable value
L <- 9
L <- 3

m <- (L-1)/2


par(mfrow=c(2,1))

# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
speech.ave <- astsa::mvspec(speech, kernel("daniell", m), log = "no")

speech.ave <- astsa::mvspec(speech, kernel("daniell", m), log = "yes")



# ------------------------------------------------------------------------------
# bandwidth and degrees of freedom
# ------------------------------------------------------------------------------

# bandwidth
speech.ave$bandwidth


# df:  adjusted degrees of freedom = 2 * L * 1020 / 1024 ~ 18
( df <- speech.ave$df )



