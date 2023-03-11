# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "fracdiff", "cluster")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  beamd
#  - infrasonic or low-frequency acoustic signal from a nuclear explosion, observed on a small trangular array of N=3 acoustic sensors
#  - Because of the way signals propagate, a plane wave signal of this kind, from a given source, traveling at a given verocity,
#   will arrive at elements in the array at predictable time delays
#  - Each series contain n = 2048 points, sampled at 10 points per second.
# ------------------------------------------------------------------------------
data(beamd)
str(beamd)

attach(beamd)

MTSplot(beamd)



# ------------------------------------------------------------------------------
# Cross-Correlation and time delays
# ------------------------------------------------------------------------------
par(mfrow=c(2,1))
ccf(sensor1, sensor2)
ccf(sensor3, sensor2)


# time delays: sensor2 delays 17 compared to sensor1
u = ccf(sensor1, sensor2, plot=FALSE)
u$lag[which.max(u$acf)]    #  17


# time delays: sensor2 leads 22 compared to sensor3
u = ccf(sensor3, sensor2, plot=FALSE)
u$lag[which.max(u$acf)]    # -22



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
tau    = rep(0,3) 
tau[1] = 17
tau[3] = -22


# beam is computed based on average of individual channels.
# The noise has been reduced and the essential characteristics of the common signal are retained in the average.
Y     = ts.union(stats::lag(sensor1,tau[1]), stats::lag(sensor2, tau[2]), stats::lag(sensor3, tau[3]))
Y     = ts.union(Y, rowMeans(Y))
colnames(Y) <- c("sensor1", "sensor2", "sensor3", "beam")
Time  = time(Y)

graphics.off()
par(mfrow=c(4,1), mar=c(0, 3.1, 0, 1.1), oma=c(2.75, 0, 2.5, 0), mgp=c(1.6,.6,0))

plot(Time, Y[,1], ylab='sensor1', xaxt="no", type='n')
grid(); lines(Y[,1])
title(main="Infrasonic Signals and Beam", outer=TRUE)

plot(Time, Y[,2], ylab='sensor2', xaxt="no",  type='n')
grid(); lines(Y[,2]) 

plot(Time,Y[,3], ylab='sensor3', xaxt="no", type='n')
grid(); lines(Y[,3])

plot(Time, Y[,4], ylab = "beam", type='n')
grid(); lines(Y[,4])

title(xlab="Time", outer=TRUE)



# ------------------------------------------------------------------------------
# Detecting common signal for the 3 series using ANOPOW
# ------------------------------------------------------------------------------
attach(beamd)
L = 9
N = 3
Y = cbind(beamd, beam=rowMeans(beamd))
( n = nextn(nrow(Y)) )


Y.fft = mvfft(as.ts(Y))/sqrt(n)
Df = Y.fft[,1:3]   # fft of the data
Bf = Y.fft[,4]     # beam fft 


ssr = N*Re(Bf*Conj(Bf))               # raw signal spectrum
sse = Re(rowSums(Df*Conj(Df))) - ssr  # raw error spectrum


# Smooth
SSE = stats::filter(sse, sides=2, filter=rep(1/L,L), circular=TRUE)
SSR = stats::filter(ssr, sides=2, filter=rep(1/L,L), circular=TRUE)
SST = SSE + SSR


eF = (N-1)*SSR/SSE; df1 = 2*L;  df2 = 2*L*(N-1)
pvals = pf(eF, df1, df2, lower=FALSE)  # p values for FDR
fdr <- 0.001
pID = FDR(pvals, fdr); Fq = qf(1-fdr, df1, df2)  


# Analysis of power components due to error (dashed line) and the total power (solid line)
# The power is maximum at about 0.002 cycles per point or about 0.02 cycles per second.
# F-statistics shows detections with the dashed line based on an false detection rate level of 0.001,
# and the solid line corresponding null F quantile.
# Little power of consequence appears to exist elsewhere, however, there is some marginally significant signal power near the 0.5 cycles per second frequency band.
par(mfrow=c(2,1), mar=c(4,4,2,1)+.1)
Fr  = 0:(n-1)/n
nFr = 1:200   # freqs to plot

plot( Fr[nFr], SST[nFr], type="l", ylab="log Power", xlab="", main="Sum of Squares",log="y")
lines(Fr[nFr], SSE[nFr], type="l", lty=2)

plot(Fr[nFr], eF[nFr], type="l", ylab="F-statistic", xlab="Frequency",  main="F Statistic")
abline(h=c(Fq, eF[pID]), lty=1:2)

