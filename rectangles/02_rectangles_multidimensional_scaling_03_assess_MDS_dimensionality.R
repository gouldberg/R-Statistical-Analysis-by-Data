setwd("//media//kswada//MyFiles//R//rectangles")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rectangles
# ------------------------------------------------------------------------------

data(rectangles, package = "smacof")


str(rectangles)


car::some(rectangles)



# ------------------------------------------------------------------------------
# Assess MDS dimensionaly by global stress
# ------------------------------------------------------------------------------

# Simulate global stress and random stress by dimension
stress_bydim <- c()
ranstress_bydim_med <- c()
ranstress_bydim_q5 <- c()
ranstress_bydim_q95 <- c()



maxdim <- 4

for(ndim in 1:maxdim){
  res <- mds(delta = diss_mx, ndim = ndim, type = "mspline")$stress
  stress_bydim <- c(stress_bydim, res)
  
  ex <- randomstress(n = 16, ndim = ndim, nrep = 100, type = "mspline")
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
# 2 dimension solution is reasonable

