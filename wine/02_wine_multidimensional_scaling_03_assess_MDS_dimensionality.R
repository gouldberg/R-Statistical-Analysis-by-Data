setwd("//media//kswada//MyFiles//R//wine")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wine
#   - 
# ------------------------------------------------------------------------------

wine <- read.table("wine.csv", header = TRUE, row.names = 1, sep = ";", check.names = FALSE)


# this produces error
str(wine)


# data("wine", package = "FactoMineR")

dim(wine)

head(wine)



# ------------------------------------------------------------------------------
# Assess MDS dimensionality by global stress
# ------------------------------------------------------------------------------

# Simulate global stress and random stress by dimension
stress_bydim <- c()

ranstress_bydim_med <- c()

ranstress_bydim_q5 <- c()

ranstress_bydim_q95 <- c()



# we try for distmat2
diss <- distmat2

maxdim <- 5

for(ndim in 1:maxdim){
  stress <- mds(delta = diss, ndim = ndim, type = "ordinal")$stress
  stress_bydim <- c(stress_bydim, stress)
  
  ex <- randomstress(n = dim(diss)[1], ndim = ndim, nrep = 100, type = "ordinal")
  ranstress_bydim_med <- c(ranstress_bydim_med, median(ex))
  ranstress_bydim_q5 <- c(ranstress_bydim_q5, unname(quantile(ex, 0.05)))
  ranstress_bydim_q95 <- c(ranstress_bydim_q95, unname(quantile(ex, 0.95)))
}



# ----------
graphics.off()

par(mfrow=c(1,1))
plot(1:maxdim, stress_bydim, type = "b", lty = 1, lwd = 1, ylim = c(0, 0.50))
lines(1:maxdim, ranstress_bydim_med, lty = 1, lwd = 1, col = "gray")
lines(1:maxdim, ranstress_bydim_q5, lty = 2, lwd = 1, col = "gray")
lines(1:maxdim, ranstress_bydim_q95, lty = 2, lwd = 1, col = "gray")



# -->
# 1 - 2 dimensions solution is reasonable for this MDS



