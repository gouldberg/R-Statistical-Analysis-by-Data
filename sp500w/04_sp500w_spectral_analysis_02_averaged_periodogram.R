setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)



# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

L1 <- 3
L2 <- 52*1+1
L3 <- 52*2+1
L4 <- 52*4+1
L5 <- 52*8+1


par(mfrow=c(2,3))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1

astsa::mvspec(sp500w, kernel("daniell", (L1-1)/2), log = "no")
astsa::mvspec(sp500w, kernel("daniell", (L2-1)/2), log = "no")
astsa::mvspec(sp500w, kernel("daniell", (L3-1)/2), log = "no")
astsa::mvspec(sp500w, kernel("daniell", (L4-1)/2), log = "no")
astsa::mvspec(sp500w, kernel("daniell", (L5-1)/2), log = "no")



# -->
# While changing bandwidth, the spectrum is also changing ... even though at same cycle
