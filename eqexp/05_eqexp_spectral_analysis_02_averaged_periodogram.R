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
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

# Trying values of L leads to the choice L = 21
L <- 21

m <- (L-1)/2


graphics.off()
par(mfrow=c(1,2))

# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
lapply(1:5, function(k){
  eqp.ave <- astsa::mvspec(x[,k], kernel("daniell", m), log = "no", main = paste0("P : ", x.name[k]))
  eqs.ave <- astsa::mvspec(x[,k+1], kernel("daniell", m), log = "no", main = paste0("S: ", x.name[k+1]))
})



# ------------------------------------------------------------------------------
# bandwidth
# ------------------------------------------------------------------------------

eqp.ave$bandwidth


# -->
# The displayed bandwidth (0.025) is adjusted for the fact that the frequency scale of the plot is in terms of cycles per ...
# The bandwidth in terms of months is L / 1024 = 0.0205


