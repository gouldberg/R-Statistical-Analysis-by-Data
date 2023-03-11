setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
#   - Various bivariate earthquakes (EQ) and explosions (EX) recorded at 40 pts/s including an even NZ (Novaya Zemlya)
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)


# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     eqexp data has exactly 1024 observartions, and 1024 pts will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(x[,1]))


graphics.off()
par(mfrow=c(2,2))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling

for(k in 1:5){
  astsa::mvspec(x[,k], log = "no", main = paste0(colnames(x)[k], ": raw periodogram"))
  astsa::mvspec(x[,k+5], log = "no", main = paste0(colnames(x)[k+5], ": raw periodogram"))
  astsa::mvspec(x[,k], log = "yes", main = paste0(colnames(x)[k], ": logged"))
  astsa::mvspec(x[,k+5], log = "yes", main = paste0(colnames(x)[k+5], ": logged"))
}



# -->
# Explosions have much high frequencies compared to earthquakes

# But it seems to be difficult to discriminate NZ waves because it has wide range of frequencies compared to explosions,
# while it does not have much low frequencies in P wave.

