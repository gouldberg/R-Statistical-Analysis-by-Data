setwd("//media//kswada//MyFiles//R//tecator")

# packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "fda.usc")
packages <- c("dplyr", "caret", "lattice", "fda.usc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tecator
#   - Infrated (IR) spectroscopy technology is used to determine the chemical makeup of substance. The theory of IR spectroscopy holds that unique
#     molecular structures absorb IR frequencies differently.
#     In practice a spectrometer fires a series of IR frequencies into a sample material, and the device measures the absorbance of the sample at each
#     individual frequency.
#     This series of measurements creates a spectrum profile which can then be used to determine the chemical makeup of the sample material.
#   - A Tecator Infratec Food and Feed Analyzer instrument was used to analyze 215 samples of meat across 100 frequencies.
#     In addition to an IR profile, analytical chemistry determined the percent content of water, fat, and protein for each sample.
#   - If we can establish a predictive relationship between IR spectrum and fat content, then food scientists could predict a sample's fat content with IR
#     instead of using analytical chemistry.
#     
#   - The matrix "absorp" contains the 100 absorbance values for the 215 samples,
#     while matrix endpoints contains the percent of moisture, fat, and protein in columns 1-3, respectively.
#   - The predictors are the measurements at the individual frequencies. Because the frequencies lie in a systematic order (850-1,050 nm), 
#     the predictors have a high degree of correlation. Hence, the data lie in a smaller dimension than the total number of predictors (215).
# ------------------------------------------------------------------------------
data("tecator", package = "caret")
# data("tecator", package = "fda.usc")

dim(absorp)
dim(endpoints)

car::some(absorp)
car::some(endpoints)



# ------------------------------------------------------------------------------
# plot 1 - 50 samples by sampled frequencies
# ------------------------------------------------------------------------------
idx <- sample(1:ncol(absorp), size = 5, replace = FALSE)

par(mfrow=c(1,1))
matplot(absorp[1:50, idx], pch = 1:4, type = "o")


# -->
# Because the frequencies lie in a systematic order (850 - 1,050 nm), the predictors have a high degree of correlation.


# ----------
# all samples
matplot(t(absorp), type = "l", xlab = "Wavelength", ylab = "Spectrum")
