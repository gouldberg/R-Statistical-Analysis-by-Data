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
# Analysis of Power Tests
#  - We would like to examine these differences further by testing whether the mean differences are because of the nature of the stimulus
#    or the consciousness level, or perhaps due to an interaction between the two factors.
#  - Unequal numbers of observations exist in the cells that contributed the means. We put the model in the form of the regression model.
# ------------------------------------------------------------------------------

n = 128                         # length of series

n.freq = 1 + n/2                # number of frequencies

Fr = (0:(n.freq-1))/n           # the frequencies 

nFr = 1:(n.freq / 2)

n.para = 6                      # number of parameters

df.stm = 2*L*(3-1)              # stimulus (3 levels: Brush,Heat,Shock)

df.con = 2*L*(2-1)              # conscious (2 levels: Awake,Sedated)  

df.int = 2*L*(3-1)*(2-1)        # interaction 

den.df = 2*L*(n.subject-n.para) # df for full model 




# ----------
# regression:  y(t) = mu(t) + a1(t) + a2(t) + b(t) + g1(t) + g2(t) + error
# mu: the sum of a mean
# a: row effect, type of stimulus
# b: column effect, level of consciousness
# g: interaction
# Design Matrix:          mu  a1  a2   b  g1  g2

# Awake: Brush(Z1), Heat(Z2) and Shock(Z3)
Z1 = outer(rep(1,N[1]), c(1,  1,  0,  1,  1,  0)) 
Z2 = outer(rep(1,N[2]), c(1,  0,  1,  1,  0,  1)) 
Z3 = outer(rep(1,N[3]), c(1, -1, -1,  1, -1, -1)) 


# Low anesthesia: Brush(Z4), Heat(Z5) and Shock(Z6)
Z4 = outer(rep(1,N[4]), c(1,  1,  0, -1, -1,  0)) 
Z5 = outer(rep(1,N[5]), c(1,  0,  1, -1,  0, -1)) 
Z6 = outer(rep(1,N[6]), c(1, -1, -1, -1,  1,  1))

Z = rbind(Z1, Z2, Z3, Z4, Z5, Z6)  



# ----------
ZZ = t(Z) %*% Z

rep(NA, n)-> SSEF -> SSE.stm -> SSE.con -> SSE.int              

HatF    = Z %*% solve(ZZ,t(Z))     

Hat.stm = Z[,-(2:3)] %*% solve(ZZ[-(2:3),-(2:3)], t(Z[,-(2:3)]))  

Hat.con = Z[,-4] %*% solve(ZZ[-4,-4], t(Z[,-4]))   

Hat.int = Z[,-(5:6)] %*% solve(ZZ[-(5:6),-(5:6)], t(Z[,-(5:6)]))                                                            




# ----------
graphics.off()

par(mfrow=c(5,3),mar=c(3.5,4,0,0),oma=c(0,0,2,2),mgp = c(1.6,.6,0)) 

loc.name = c("Cortex 1","Cortex 2","Cortex 3","Cortex 4","Caudate", "Thalamus 1","Thalamus 2",
             "Cerebellum 1","Cerebellum 2")

for(Loc in c(1:4,9)) {   # only Loc 1 to 4 and 9 used                
  i = 6*(Loc-1)                                                                 
  Y = cbind(fmri[[i+1]], fmri[[i+2]], fmri[[i+3]], fmri[[i+4]], fmri[[i+5]], fmri[[i+6]])                     
  
  Y = mvfft(spec.taper(Y, p=.5))/sqrt(n)  
  
  Y = t(Y)  
  for (k in 1:n) {    
    SSY = Re(Conj(t(Y[,k])) %*% Y[,k])
    
    SSReg = Re(Conj(t(Y[,k])) %*% HatF %*% Y[,k])
    SSEF[k] = SSY - SSReg 
    
    SSReg = Re(Conj(t(Y[,k])) %*% Hat.stm %*% Y[,k])
    SSE.stm[k] = SSY - SSReg
    
    SSReg = Re(Conj(t(Y[,k])) %*% Hat.con %*% Y[,k])
    SSE.con[k] = SSY - SSReg  
    
    SSReg = Re(Conj(t(Y[,k])) %*% Hat.int %*% Y[,k])
    SSE.int[k] = SSY - SSReg   
  }
  
  
  # Smooth    
  sSSEF    = stats::filter(SSEF, rep(1/L, L), circular = TRUE)
  sSSE.stm = stats::filter(SSE.stm, rep(1/L, L), circular = TRUE)
  sSSE.con = stats::filter(SSE.con, rep(1/L, L), circular = TRUE)
  sSSE.int = stats::filter(SSE.int, rep(1/L, L), circular = TRUE)  
  eF.stm   = (den.df/df.stm)*(sSSE.stm-sSSEF)/sSSEF
  eF.con   = (den.df/df.con)*(sSSE.con-sSSEF)/sSSEF
  eF.int   = (den.df/df.int)*(sSSE.int-sSSEF)/sSSEF
  
  
  plot(Fr[nFr], eF.stm[nFr], type="l", xlab="Frequency", ylab="F Statistic", ylim=c(0,12))
  abline(h=qf(.999, df.stm, den.df),lty=2)       
  if(Loc==1) mtext("Stimulus", side=3, line=.3, cex=1)
  mtext(loc.name[Loc], side=2, line=3, cex=.9)
  
  plot(Fr[nFr], eF.con[nFr], type="l", xlab="Frequency", ylab="F Statistic", ylim=c(0,12))
  abline(h=qf(.999, df.con, den.df),lty=2)
  if(Loc==1)  mtext("Consciousness", side=3, line=.3, cex=1)
  
  plot(Fr[nFr], eF.int[nFr], type="l", xlab="Frequency", ylab="F Statistic", ylim=c(0,12))
  abline(h=qf(.999, df.int, den.df),lty=2)
  if(Loc==1) mtext("Interaction", side=3, line= .3, cex=1)   
} 



# -->
# The state of consciousness (Awake, Sedated) has the major effect at the signal frequency
# The level of stimulus was less significatn at the signal frequency.
# A significant interaction occurred, however, at the ipsilateral component of the primary somatosensory cortex location

