setwd("//media//kswada//MyFiles//R//fmri1")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fMRI Imaging
# ------------------------------------------------------------------------------

data(fmri1, package = "astsa")

str(fmri1)

head(fmri1)


data(fmri, package = "astsa")

str(fmri)

head(fmri)



# ------------------------------------------------------------------------------
# Individual periodogram
# ------------------------------------------------------------------------------

n = 128


# The individual periodogram
Per = abs(mvfft(fmri1[,-1]))^2/n


graphics.off()

par(mfrow=c(2,4), mar=c(3,2,2,1),  mgp = c(1.6,.6,0), oma=c(0,1,0,0))

for (i in 1:8) plot(0:20, Per[1:21,i], type="l", ylim=c(0,8), main=colnames(fmri1)[i+1], 
                    xlab="Cycles", ylab="", xaxp=c(0,20,5))
mtext("Periodogram", side=2, line=-.3, outer=TRUE, adj=c(.2,.8))



# -->
# As expected, a strong response to the brush stimulus occurred in areas of the cortex.



# ------------------------------------------------------------------------------
# Estimate spectral density
# ------------------------------------------------------------------------------

fxx = mvspec(fmri1[,-1], kernel("daniell", c(1,1)), taper=.5, plot=FALSE)$fxx


l.val = rep(NA,64)

for (k in 1:64) {
  u = eigen(fxx[,,k], symmetric=TRUE, only.values=TRUE)
  l.val[k] = u$values[1]
}



# -->
# we will focus primarily on the signal period of 64s, which translates to 4 cycles in 256s or omega = 4/128 = 32 cycles per time point
# Estimated spectrum of the 1st principal component series by calculating the largest eigenvalue for each j = 0,1,2,...,64.



# largest peak (=2) at the stimulus frequency 4/128
par(mfrow=c(1,1))
plot(l.val, type="l", xaxp=c(0,64,8), xlab="Cycles (Frequency x 128)", ylab="First Principal Component")
axis(1, seq(4,60,by=8), labels=FALSE)


