setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\gasfurnace")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
#   - Series of input and output measurements on a fas furnace that have been previously modeled by spectral analysis
#     in Jenkins and Watts (1968), and by lagged correlation methods in Box and Jenkins (1970).
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ------------------------------------------------------------------------------
# Spectral analysis:  Coherence between SOI and Recruitment
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
#        An important example of the application of the cross-spectrum is to the problem of predicting an output series y(t) from
#        some input series x(t) through a linear filter relation such as the three-point moving average.
#        A measure of the strength of such a relation is the squared coherence.
# ------------------------------------------------------------------------------

input <- gasf$input
output <- gasf$output


input <- diff(gasf$input)
output <- diff(gasf$output)



# L <- 9
L <- 19

( m <- (L - 1) / 2 )


sr <- mvspec(cbind(input, output), kernel("daniell", m), plot=FALSE)




# ----------
sr$df


# significance level alpha = 0.001
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(2,1))

plot(sr)

plot(sr, plot.type = "coh", ci.lty = 2)

abline(h = C)


