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
# ------------------------------------------------------------------------------

L <- 9

fdr <- 0.001

N <- 3


Y <- cbind(beamd, beam = rowMeans(beamd))

n <- nextn(nrow(Y))


# ----------
Y.fft <- mvfft(as.ts(Y)) / sqrt(n)

Df <- Y.fft[,1:3]

Bf <- Y.fft[,4]


# ----------
# Raw signal spectrum and raw error spectrum
ssr <- N * Re(Bf * Conj(Bf))

sse <- Re(rowSums(Df * Conj(Df))) - ssr



# ----------
# Smoothing
SSE <- stats::filter(sse, sides = 2, filter = rep(1/L, L), circular = TRUE)

SSR <- stats::filter(ssr, sides = 2, filter = rep(1/L, L), circular = TRUE)

SST <- SSE + SSR



# ----------
# Because only 3 elements in the array and a reasonable number of points in time exist,
# it seems advisable to employ some smoothing over frequency to obtain additional degress of freedom.


# F-statostic
( eF <- (N - 1) * SSR / SSE )


# degree of freedoms
( df1 <- 2 * L )

( df2 <- 2 * L * (N - 1) )


# p values for FDR
( pvals <- pf(eF, df1, df2, lower = FALSE) )


# FDR:  compute the basic false discovery rate given a vector of p-values
( pID <- FDR(pvals, fdr) )


( Fq <- qf(1 - fdr, df1, df2) )



# fundamental frequencies
Fr <- 0:(n-1)/n


# number of freqs to plot
nFr <- 1:200



# ----------
par(mfrow = c(2,1), mar = c(4,4,2,1) + 0.1)

plot(Fr[nFr], SST[nFr], type = "l", ylab = "log Power", xlab = "", main = "Sum of Squares", log = "y")

lines(Fr[nFr], SSE[nFr], type = "l", lty = 2)

plot(Fr[nFr], eF[nFr], type = "l", ylab = "F-statistic", xlab = "Frequency", main = "F Statistic")

abline(h = c(Fq, eF[pID]), lty = 1:2)



# -->
# Top panel:  Analysis of power for infrasond array on a log scale with SST(omega) shown as a solid line and SSE(omega) as dashed line
# Bottom panel:  The F-statistics showing detections with the dashed line based on an FDR level of 0.001 and the solid line corresponding null F quantile




