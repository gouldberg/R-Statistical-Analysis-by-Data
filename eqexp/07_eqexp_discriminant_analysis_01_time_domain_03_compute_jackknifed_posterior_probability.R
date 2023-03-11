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
# Compute posterior probability by cross-validated discrimant functions
# ------------------------------------------------------------------------------

# Cross-validation (jackknife procedure)
# holding out the observation to be classified, deriving the classification function from the remaining observations

all.data <- rbind(cbind(eq.P, eq.S), cbind(ex.P, ex.S))

post.eq <- rep(NA, 8) -> post.ex

for(j in 1:16){
  if(j <= 8){ samp.eq = all.data[-c(j, 9:16),];  samp.ex = all.data[9:16,] }
  if(j > 8){ samp.eq = all.data[1:8,];  samp.ex = all.data[-c(j, 1:8),] }
  df.eq <- nrow(samp.eq) - 1
  df.ex <- nrow(samp.ex) - 1
  mean.eq <- colMeans(samp.eq)
  mean.ex <- colMeans(samp.ex)
  cov.eq <- var(samp.eq)
  cov.ex <- var(samp.ex)
  cov.pooled <- (df.eq * cov.eq + df.ex * cov.ex) / (df.eq + df.ex)
  slopes.eq <- solve(cov.pooled, mean.eq)
  inter.eq <- -sum(slopes.eq * mean.eq)/2
  slopes.ex <- solve(cov.pooled, mean.ex)
  inter.ex <- -sum(slopes.ex * mean.ex)/2
  d.slopes <- slopes.eq - slopes.ex
  d.inter <- inter.eq - inter.ex
  d <- sum(d.slopes * all.data[j,]) + d.inter
  if(j <= 8) post.eq[j] <- exp(d) / (1 + exp(d))
  if(j > 8) post.ex[j - 8] <- 1 / ( 1 + exp(d) )
}



# ----------
Posterior <- cbind(1:8, post.eq, 1:8, post.ex)

colnames(Posterior) <- c("EQ", "P(EQ|data)", "EX", "P(EX|data)")



# Results from Cross-validation 
round(Posterior, 3)


# -->
# The jackknifed posterior probabilities of being an earthquake for the earthquake group ranged from 0.621 to 1.000,
# where as the explosion probabilities for the explosion group ranged from 0.717 to 1.000
# The unknown event, NZ, is classified as an explosion, with posterior probability 0.960.


