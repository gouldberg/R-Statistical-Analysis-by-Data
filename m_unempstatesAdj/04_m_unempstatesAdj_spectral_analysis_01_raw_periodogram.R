# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_unempstatesAdj")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-unempstatesAdj
# ------------------------------------------------------------------------------

da <- read.csv("m-unempstatesAdj.txt", sep = "", header = T)


str(da)


dim(da)


car::some(da)



# ----------
# first difference

drate <- diffM(da)




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     SOI data has 453 observartions, and 480 months will be used in the spectral analyses by default
# ------------------------------------------------------------------------------


nextn(nrow(drate))




# -->
# actually data is only 415 points
# now the data is padded and recognized as a series of length 432




# ----------
graphics.off()

par(mfrow=c(2,3))

# for(i in 2:11){ astsa::mvspec(mtenstocks[,i], log = "no", main = paste0(colnames(mtenstocks)[i])) }

for(i in 1:5){ astsa::mvspec(drate[,i], log = "no", main = paste0(colnames(drate)[i])) }


# -->
# frequency bandwidth = 0.00231:  1 unit along frequency axis has 1 / 0.00231 = 433 points
# the data is 1 month in 1 cycle --> fequency axis is labeled in multiples of 1
# frequency axis "1" is 433 points,  "0.1" is 43.3 month cycle



