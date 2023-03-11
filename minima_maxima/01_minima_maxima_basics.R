setwd("//media//kswada//MyFiles//R//minima_maxima")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  minima and maxima
#   - Generate 1000 samples, each of length n = 1000, from a normal distribution with mu = 10 and sigma = 2
#     For each sample calculate its median, minimum, and maximum values
# ------------------------------------------------------------------------------

Ymin <- Ymax <- Ymid <- rep(0, 1000)


for(i in 1:1000){
  
  Sample <- rNO(1000, mu = 10, sigma = 2)
  
  Ymax[i] <- max(Sample)
  
  Ymin[i] <- min(Sample)
  
  Ymid[i] <- median(Sample)
}



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

par(mfrow=c(1,3), mar = c(2,2,2,2))

MASS::truehist(Ymax, main = "max")

MASS::truehist(Ymin, main = "min")

MASS::truehist(Ymid, main = "median")



# ----------
psych::describe(Ymax)

psych::describe(Ymin)

psych::describe(Ymid)



