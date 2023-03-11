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
# Frequency-dependent equality of means test for fMRI data at 9 brain locations
# ------------------------------------------------------------------------------

n          = 128               # length of series
n.freq     = 1 + n/2           # number of frequencies
Fr         = (0:(n.freq-1))/n  # the frequencies 
N          = c(5,4,5,3,5,4)    # number of series for each cell
n.subject  = sum(N)            # number of subjects (26)
n.trt      = 6                 # number of treatments
L          = 3                 # for smoothing
num.df     = 2*L*(n.trt-1)     # dfs for F test
den.df     = 2*L*(n.subject-n.trt)



# ----------
# Design Matrix (Z): 
Z1 = outer(rep(1,N[1]), c(1,1,0,0,0,0))
Z2 = outer(rep(1,N[2]), c(1,0,1,0,0,0))
Z3 = outer(rep(1,N[3]), c(1,0,0,1,0,0))
Z4 = outer(rep(1,N[4]), c(1,0,0,0,1,0)) 
Z5 = outer(rep(1,N[5]), c(1,0,0,0,0,1)) 
Z6 = outer(rep(1,N[6]), c(1,-1,-1,-1,-1,-1)) 


# 26 subjects * 6 treatments
Z  = rbind(Z1, Z2, Z3, Z4, Z5, Z6)



# ----------
# 6 * 6
ZZ = t(Z) %*% Z 


SSEF <- rep(NA, n) -> SSER   

HatF = Z %*% solve(ZZ, t(Z))

HatR = Z[,1] %*% t(Z[,1])/ZZ[1,1]



# ----------
graphics.off()

par(mfrow=c(3,3), mar=c(3.5,4,0,0), oma=c(0,0,2,2), mgp = c(1.6,.6,0))

loc.name = c("Cortex 1","Cortex 2","Cortex 3","Cortex 4","Caudate","Thalamus 1","Thalamus 2",
             "Cerebellum 1","Cerebellum 2")

for(Loc in 1:9) {   
  i = n.trt*(Loc-1)   
  Y = cbind(fmri[[i+1]], fmri[[i+2]], fmri[[i+3]], fmri[[i+4]], fmri[[i+5]], fmri[[i+6]])
  
  # spec.taper:  Taper a time series by a cosine bell
  # mvfft:  multivariate DFT by FFT
  Y = mvfft(spec.taper(Y, p=.5))/sqrt(n)	
  Y = t(Y)      # Y is now 26 x 128 FFTs
  
  # Calculation of Error Spectra 
  for (k in 1:n) {   
    SSY = Re(Conj(t(Y[,k])) %*% Y[,k])
    SSReg = Re(Conj(t(Y[,k])) %*% HatF %*% Y[,k])
    SSEF[k] = SSY - SSReg
    SSReg = Re(Conj(t(Y[,k])) %*% HatR %*% Y[,k])
    SSER[k] = SSY - SSReg  
  }
  
  # Smooth 
  sSSEF = stats::filter(SSEF, rep(1/L, L), circular = TRUE)
  sSSER = stats::filter(SSER, rep(1/L, L), circular = TRUE)
  
  # F Statistic
  eF = (den.df / num.df) * (sSSER - sSSEF) / sSSEF
  
  plot(Fr, eF[1:n.freq], type="l", xlab="Frequency", ylab="F Statistic", ylim=c(0,7))
  abline(h=qf(.999, num.df, den.df),lty=2)
  text(.25, 6.5, loc.name[Loc], cex=1.2)   
}


# -->
# We see substantial signals for the 4 cortex locations and for the 2nd cerebellum trace,
# but the effects are nonsignificant in the caudate and thalamus regions



# ------------------------------------------------------------------------------
# Visual check  -->  MORE NEED TO REFINE
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))

cortex1 <- cbind(fmri$L1T1, fmri$L1T2, fmri$L1T3, fmri$L1T4, fmri$L1T5, fmri$L1T6)
ts.plot(cortex1, col = 1:n.subject)


caudate <- cbind(fmri$L5T1, fmri$L5T2, fmri$L5T3, fmri$L5T4, fmri$L5T5, fmri$L5T6)
ts.plot(caudate, col = 1:n.subject)



