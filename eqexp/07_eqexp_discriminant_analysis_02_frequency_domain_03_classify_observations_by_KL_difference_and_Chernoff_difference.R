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
# Discriminant analysis:  Frequency Domain
#   - Calculate the difference of KL divergence and Chernoff disparity measure
# ------------------------------------------------------------------------------

# calculate information criteria
# and differences between earthquakes and explosions

rep(0,17) -> KLDiff -> BDiff -> KLeq -> KLex -> Beq -> Bex


for (i in 1:17){
  
  if (i <= 8) f0 = f.eq[i,,,]
  
  if (i > 8 & i <= 16) f0 = f.ex[i-8,,,]
  
  if (i == 17) f0 = f.NZ
  
  for (k in 1:256) {    # only use freqs out to .25
    tr = Re(sum(diag(solve(fhat.eq[,,k],f0[,,k]))))
    
    KLeq[i] = KLeq[i] + tr + log(det.c(fhat.eq[,,k])) - log(det.c(f0[,,k]))
    
    Beq[i] =  Beq[i] + Re(log(det.c(alf * f0[,,k] + (1 - alf) * fhat.eq[,,k]) / det.c(fhat.eq[,,k])) - 
                            alf * log(det.c(f0[,,k]) / det.c(fhat.eq[,,k])))
    
    tr = Re(sum(diag(solve(fhat.ex[,,k], f0[,,k]))))
    
    KLex[i] = KLex[i] + tr +  log(det.c(fhat.ex[,,k])) - log(det.c(f0[,,k]))
    
    Bex[i] = Bex[i] + Re(log(det.c(alf * f0[,,k] + (1 - alf) * fhat.ex[,,k]) / det.c(fhat.ex[,,k])) - 
                           alf * log(det.c(f0[,,k]) / det.c(fhat.ex[,,k]))) 
  }
  
  KLDiff[i] = (KLeq[i] - KLex[i]) / n 
  
  BDiff[i] =  (Beq[i] - Bex[i]) / (2 * n) 
}


x.b = max(KLDiff) + .1; x.a = min(KLDiff) - .1

y.b = max(BDiff) + .01; y.a = min(BDiff) - .01




# ------------------------------------------------------------------------------
# plot Chernoff Differences vs. KL Differences
# ------------------------------------------------------------------------------

# The NZ event of unknown origin is classified as an explosion

par(mfrow=c(1,1))

plot(KLDiff[9:16], BDiff[9:16], type="p", xlim=c(x.a,x.b), ylim=c(y.a,y.b), cex=1.1, lwd=2, 
     xlab="Kullback-Leibler Difference",ylab="Chernoff Difference", main="Classification 
     Based on Chernoff and K-L Distances", pch=6)

points(KLDiff[1:8], BDiff[1:8], pch=8, cex=1.1, lwd=2)
points(KLDiff[17], BDiff[17],  pch=3, cex=1.1, lwd=2)

legend("topleft", legend=c("EQ", "EX", "NZ"), pch=c(8,6,3), pt.lwd=2)
abline(h=0, v=0, lty=2, col="gray")

text(KLDiff[-c(1,2,3,7,14)]-.075, BDiff[-c(1,2,3,7,14)], label=names(eqexp[-c(1,2,3,7,14)]), cex=.7)
text(KLDiff[c(1,2,3,7,14)]+.075, BDiff[c(1,2,3,7,14)], label=names(eqexp[c(1,2,3,7,14)]), cex=.7)



