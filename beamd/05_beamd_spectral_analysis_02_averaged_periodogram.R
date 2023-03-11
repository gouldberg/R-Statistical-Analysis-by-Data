setwd("//media//kswada//MyFiles//R//beamd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  beamd
# ------------------------------------------------------------------------------

data(beamd, package = "astsa")

str(beamd)

head(beamd)


sns1 <- beamd$sensor1
sns2 <- beamd$sensor2
sns3 <- beamd$sensor3



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

# Trying values of L leads to the choice L = 9 as a reasonable value
L <- 9

m <- (L-1)/2


par(mfrow=c(3,1))

# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
sns1.ave <- astsa::mvspec(sns1, kernel("daniell", m), log = "no")
sns2.ave <- astsa::mvspec(sns2, kernel("daniell", m), log = "no")
sns3.ave <- astsa::mvspec(sns3, kernel("daniell", m), log = "no")



# ------------------------------------------------------------------------------
# bandwidth and degrees of freedom
# ------------------------------------------------------------------------------

sns1.ave$bandwidth

sns1.ave$df

