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
# Simultaneous Inference
#  - We would like to investigate which of the stimuli, heat, brushing, or shock, had the most effect.
# ------------------------------------------------------------------------------

# Design Matrix            
Z1 = outer(rep(1,N[1]), c(1,0,0,0,0,0)) 
Z2 = outer(rep(1,N[2]), c(0,1,0,0,0,0)) 
Z3 = outer(rep(1,N[3]), c(0,0,1,0,0,0)) 
Z4 = outer(rep(1,N[4]), c(0,0,0,1,0,0)) 
Z5 = outer(rep(1,N[5]), c(0,0,0,0,1,0)) 
Z6 = outer(rep(1,N[6]), c(0,0,0,0,0,1))

Z  = rbind(Z1, Z2, Z3, Z4, Z5, Z6)


# ----------
ZZ = t(Z) %*% Z 

A      = rbind(diag(1,3), diag(1,3))   # Contrasts:  6 x 3 

nq     = nrow(A)

num.df = 2*L*nq

den.df = 2*L*(n.subject-nq)                   



# ----------
HatF   = Z %*% solve(ZZ, t(Z))           # full model hat matrix   



# ----------
rep(NA, n)-> SSEF -> SSER 

eF = matrix(0,n,3)



graphics.off()
par(mfrow=c(5,3), mar=c(3.5,4,0,0), oma=c(0,0,2,2), mgp = c(1.6,.6,0)) 

loc.name = c("Cortex 1","Cortex 2","Cortex 3","Cortex 4","Caudate","Thalamus 1","Thalamus 2",
             "Cerebellum 1","Cerebellum 2")

cond.name = c("Brush", "Heat", "Shock")             

for(Loc in c(1:4,9)) {
  i = 6*(Loc-1)                                                                 
  Y = cbind(fmri[[i+1]],fmri[[i+2]],fmri[[i+3]],fmri[[i+4]], fmri[[i+5]],fmri[[i+6]])                       
  
  Y = mvfft(spec.taper(Y, p=.5))/sqrt(n); Y = t(Y)  
  
  for (cond in 1:3){
    Q = t(A[,cond])%*%solve(ZZ, A[,cond])
    HR = A[,cond]%*%solve(ZZ, t(Z))      
    for (k in 1:n){   
      SSY = Re(Conj(t(Y[,k]))%*%Y[,k])
      SSReg= Re(Conj(t(Y[,k]))%*%HatF%*%Y[,k])
      SSEF[k]= (SSY-SSReg)*Q 
      SSReg= HR%*%Y[,k]
      SSER[k] = Re(SSReg*Conj(SSReg))  
    }     
    
    # Smooth      
    sSSEF = stats::filter(SSEF, rep(1/L, L), circular = TRUE)
    sSSER = stats::filter(SSER, rep(1/L, L), circular = TRUE)       
    
    
    eF[,cond] = (den.df / num.df) * (sSSER / sSSEF)
  }                                    
  

  plot(Fr[nFr], eF[nFr,1], type="l", xlab="Frequency", ylab="F Statistic", ylim=c(0,5))
  abline(h=qf(.999, num.df, den.df),lty=2)       
  if(Loc==1) mtext("Brush", side=3, line=.3, cex=1)
  mtext(loc.name[Loc], side=2, line=3, cex=.9)

  plot(Fr[nFr], eF[nFr,2], type="l", xlab="Frequency", ylab="F Statistic", ylim=c(0,5))
  abline(h=qf(.999, num.df, den.df),lty=2)
  if(Loc==1)  mtext("Heat", side=3, line=.3, cex=1)   
  
  plot(Fr[nFr], eF[nFr,3], type="l", xlab="Frequency", ylab="F Statistic", ylim=c(0,5))
  abline(h = qf(.999, num.df, den.df) ,lty=2)
  if(Loc==1) mtext("Shock", side=3, line=.3, cex=1)  
}



# -->
# We see that, at the 1st and 3rd Cortex locations, brush and heat are both significant,
# where as the 4th Cortex shows only brush and the 2nd Cerebellum shows only heat.
# Shock appears to be tranmitted relatively weakly, when averaged over the awake and mildy anesthetized states.

