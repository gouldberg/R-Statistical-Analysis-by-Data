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
# select regularizing parameter (alpha)
# ------------------------------------------------------------------------------

# choose alpha (regularizing parameter, when alpha = 0.5, the Chernoff measure is the symmetric divergence)
# Balpha: large sample spectral approximation to the Chernoff information measure

det.c = function(mat){return(Re(mat[1,1] * mat[2,2] - mat[1,2] * mat[2,1]))}


# folding frequency is 20Hz
Balpha = rep(0,19) 

for(i in 1:19){
  alf = i / 20
  for(k in 1:256) {    # only use freqs out to .25
    Balpha[i]= Balpha[i] + Re(log(det.c(alf * fhat.ex[,,k] + (1 - alf) * fhat.eq[,,k]) / det.c(fhat.eq[,,k])) - 
                                alf * log(det.c(fhat.ex[,,k]) / det.c(fhat.eq[,,k])))
  }
}


# ----------
par(mfrow = c(1,1))
plot(Balpha, type = "l")



# For the case of 2 groups, alpha should be chosen to maximize the disparity between the two group spectra
( alf = which.max(Balpha) / 20 )



# -->
# The maximum value of the estimated Chernoff disparity occurs for alpha = 0.4
# and we use that value in the dicriminant criterion.

