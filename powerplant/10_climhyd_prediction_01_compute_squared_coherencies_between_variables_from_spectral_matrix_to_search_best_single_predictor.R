setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)



# ------------------------------------------------------------------------------
# Spectral Matrix
# ------------------------------------------------------------------------------

Y <- climhyd


# log inflow and sqrt precipitation
Y[,6] <- log(Y[,6])

Y[,5] <- sqrt(Y[,5])



# ----------
L <- 25

# Spectral Matrix
par(mfrow = c(1,1))
Yspec <- mvspec(Y, spans = L, kernel = "daniell", detrend = TRUE, demean = FALSE, taper = 0.1)



# ----------
# effective sample size
( n <- Yspec$n.used )


# fundamental freqs
( Fr <- Yspec$freq )


# number of frequencies inputs (Temp and Precip)
( n.freq <- length(Fr) )


# 0.05 - the bandwidth
( Yspec$bandwidth * sqrt(12) )



# ------------------------------------------------------------------------------
# Squared Coherencies between Lake Shasta inflow and other variables
# in order to look for the best single input predictor
# ------------------------------------------------------------------------------

alpha <- 0.001

Fq <- qf(1 - alpha, 2, L- 2)


# 0.001 threshold corresponding to the F-statistic, separately, for each possible predictor of inflow.
cn <- Fq / (L - 1 + Fq)



plt.name <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")

par(mfrow = c(2,3),  cex.lab = 1.2)
# The coherencies are listed as 1,2,...,15 = choose(6,2)
for(i in 11:15){
  plot(Fr, Yspec$coh[,i], type = "l", ylab = "Sq Coherence", xlab = "Frequency", ylim = c(0,1), main = c("Inflow with", names(climhyd[i - 10])))

  abline(h = cn)
  
  text(0.45, 0.98, plt.name[i-10], cex = 1.2)
}



# -->
# Transformed (square root) precipidation produces the most consistently high squared coherence values at all frequencies (L = 25),
# with the seasonal period contributing most significantly.

# Other inputs, with the exception of wind speed, also appear to be plausible contributions.



