setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension




# ------------------------------------------------------------------------------
# Smoothing functional data using nonparameteric kernel estimation with CV
#   - In FDA (functional data analysis), we assume that there is an "true" underlying trajectory x(t(j))
#        - y(j) = x(t(j)) + error(j):  t(j) is the j-th time point
#   - The most popular is so-called Nadaraya-Watson kernel estimater, involving a smoothing aprameter (or bandwidth) h which steers the degree of smoothness.
#     The optimal h can be fnoud through cross-validaton (CV), the larger h, the smoother the curves.
# ------------------------------------------------------------------------------

# nonparametric kernel smoothing
# smoothing is performed on all individual trajectories (h constant across individuals)

# this is deprecated:
# ftensionNP <- min.np(ftension)

ftensionNP <- optim.np(ftension)


ftensionNP



# ----------
par(mfrow = c(2, 1), mar = c(2,2,2,2))

plot(ftension, main = "Original Data")

plot(ftensionNP$fdata.est, main = "Smooth Data")



# -->
# Smoothing trajectories capture important peaks and valleys of the raw input signal (i.e., not over-smoothing)




# ------------------------------------------------------------------------------
# Plot samples:  4 types  middle man, positive man, negative man, positive - negative man
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2, 2), mar = c(2,2,2,2))

plot(ftensionNP$fdata.est[c(19)], main = "middle: 19", col = c(1))

plot(ftensionNP$fdata.est[c(14,18)], main = "Positive: 14, 18", col = c(1:2))

plot(ftensionNP$fdata.est[c(10)], main = "Negative: 10", col = c(1))

plot(ftensionNP$fdata.est[c(17)], main = "positive - negative: 17", col = c(1))


