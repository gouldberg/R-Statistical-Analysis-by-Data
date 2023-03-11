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
# Calculate determinant for 2 * 2 Hermitian matrix for EX and EQ
# ------------------------------------------------------------------------------


eq = as.ts(eqexp[,1:8])

ex = as.ts(eqexp[,9:16])


f.eq <- array(dim=c(8,2,2,512)) -> f.ex 



# ----------
# below calculates determinant for 2x2 Hermitian matrix


library(astsa)


L = c(15,13,5)  # for smoothing 

for (i in 1:8){     # compute spectral matrices
  f.eq[i,,,] = mvspec(cbind(eq[P,i], eq[S,i]), spans = L, taper = .5, plot = FALSE)$fxx
  f.ex[i,,,] = mvspec(cbind(ex[P,i], ex[S,i]), spans = L, taper = .5, plot = FALSE)$fxx
}



# ------------------------------------------------------------------------------
# Calculate determinant for 2 * 2 Hermitian matrix for NZ
# ------------------------------------------------------------------------------


nz = as.ts(eqexp[,17])

f.NZ = array(dim=c(2,2,512))


par(mfrow = c(1,1))

u = mvspec(cbind(nz[P],nz[S]), spans=L, taper=.5) 



f.NZ = u$fxx	 


# about .75 Hz
( bndwidth = u$bandwidth * 40 )




# ------------------------------------------------------------------------------
# average spectra for EQ and EX
# ------------------------------------------------------------------------------

fhat.eq = apply(f.eq, 2:4, mean)

fhat.ex = apply(f.ex, 2:4, mean)




# ----------
# plot the average spectra

par(mfrow=c(2,2), mar=c(3,3,2,1),  mgp = c(1.6,.6,0))

n <- 1024

Fr = 40 * (1:512) / n

plot(Fr, Re(fhat.eq[1,1,]), type="l", xlab="Frequency (Hz)", ylab="")
plot(Fr, Re(fhat.eq[2,2,]), type="l", xlab="Frequency (Hz)", ylab="")
plot(Fr, Re(fhat.ex[1,1,]), type="l", xlab="Frequency (Hz)", ylab="")
plot(Fr, Re(fhat.ex[2,2,]), type="l", xlab="Frequency (Hz)", ylab="")

mtext("Average P-spectra", side=3, line=-1.5, adj=.2, outer=TRUE)
mtext("Earthquakes", side=2, line=-1, adj=.8,  outer=TRUE)
mtext("Average S-spectra", side=3, line=-1.5, adj=.82, outer=TRUE)
mtext("Explosions", side=2, line=-1, adj=.2, outer=TRUE)

par(fig = c(.75, 1, .75, 1), new = TRUE)
ker = kernel("modified.daniell", L)$coef
ker = c(rev(ker), ker[-1])
plot((-33:33)/40, ker,type="l", ylab="", xlab="", cex.axis=.7, yaxp=c(0,.04,2))

