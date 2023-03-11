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
# Cluster Analysis for Earthquakes and Explosions
# Compute J-Divergence measure
# ------------------------------------------------------------------------------

eq = as.ts(eqexp[,1:8])

ex = as.ts(eqexp[,9:16])

nz = as.ts(eqexp[,17])


f = array(dim=c(17,2,2,512))  

L = c(15,15)   # for smoothing 

for (i in 1:8){     # compute spectral matrices
  f[i,,,] = mvspec(cbind(eq[P,i],eq[S,i]), spans=L, taper=.5, plot=FALSE)$fxx
  f[i+8,,,] = mvspec(cbind(ex[P,i],ex[S,i]), spans=L, taper=.5, plot=FALSE)$fxx 
}


f[17,,,] = mvspec(cbind(nz[P],nz[S]), spans=L, taper=.5, plot=FALSE)$fxx	



# ----------
# calculate symmetric information criteria

# JD: quasi-distance between samples series with estimated spectral matrices based on J-Divergence

JD = matrix(0,17,17) 

for (i in 1:16){
  for (j in (i+1):17){	
    for (k in 1:256) {    # only use freqs out to .25
      tr1 = Re(sum(diag(solve(f[i,,,k], f[j,,,k]))))
      tr2 = Re(sum(diag(solve(f[j,,,k], f[i,,,k]))))
      JD[i,j] = JD[i,j] + (tr1 + tr2 - 2*p.dim)
    }
  }
}


JD = (JD + t(JD))/n

colnames(JD) = c(colnames(eq), colnames(ex), "NZ") 

rownames(JD) = colnames(JD)

