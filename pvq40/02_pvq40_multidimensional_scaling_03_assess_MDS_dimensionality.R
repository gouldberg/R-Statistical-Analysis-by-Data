setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# Assess MDS dimensionaly by global stress
# ------------------------------------------------------------------------------

# inter-correlation of the scores
( r <- cor(PVQ40, use = "pairwise.complete.obs") )



# Convert correlations to dissimilarity matrix
diss <- sim2diss(r, method = "corr") 



# ----------
# Simulate global stress and random stress by dimension
stress_bydim <- c()
ranstress_bydim_med <- c()
ranstress_bydim_q5 <- c()
ranstress_bydim_q95 <- c()



maxdim <- 4

for(ndim in 1:maxdim){
  res <- mds(delta = diss, ndim = ndim, type = "ordinal")$stress
  stress_bydim <- c(stress_bydim, res)
  
  # -- NOT RUN: TAKES TIME !!!
  # ex <- randomstress(n = dim(diss)[1], ndim = ndim, nrep = 100, type = "ordinal")
  # ranstress_bydim_med <- c(ranstress_bydim_med, median(ex))
  # ranstress_bydim_q5 <- c(ranstress_bydim_q5, unname(quantile(ex, 0.05)))
  # ranstress_bydim_q95 <- c(ranstress_bydim_q95, unname(quantile(ex, 0.95)))
}



# ----------
graphics.off()

par(mfrow=c(1,1))
plot(1:maxdim, stress_bydim, type = "b", lty = 1, lwd = 1, ylim = c(0, 0.50))
# lines(1:maxdim, ranstress_bydim_med, lty = 1, lwd = 1, col = "gray")
# lines(1:maxdim, ranstress_bydim_q5, lty = 2, lwd = 1, col = "gray")
# lines(1:maxdim, ranstress_bydim_q95, lty = 2, lwd = 1, col = "gray")



# -->
# 2 dimension solution is reasonable

