setwd("//media//kswada//MyFiles//R//intelligence")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  intelligence
# ------------------------------------------------------------------------------

data(intelligence, package = "smacof")


intelligence

car::some(intelligence)



# ------------------------------------------------------------------------------
# Assess model stability by jackknifing
# ------------------------------------------------------------------------------
idiss <- sim2diss(intelligence[,paste0("T", 1:8)])


# again fitting to recover orignal fit
fit <- mds(idiss)



# Jackknifing
JK <- jackknife(fit)


JK


# stability measure
JK$stab



# ----------
par(mfrow=c(1,1))
plot(JK, col.p = "black", col.l = "black", lwd = 1.2)



# -->
# Jackknife stability measure is hard to evaluate without benchmarks but plot(JK) exhibits the MDS configuration
# together with a star of lines attached to each point that gives you an impression of the stability of the MDS configuration under the various n - 1 conditions

# The end point of the stars show the respective point positions of the n - 1 jackknife configurations, fitted to each other,
# and subsequenctly connected to their centroid.




# ------------------------------------------------------------------------------
# Assess model stability by bootstrapping
#   - This requires a person-by-variable matrix from which the proximities are derived.
# ------------------------------------------------------------------------------

set.seed(123)


# method = "pearson" ??
resboot <- bootmds(fit, data = intelligence[,paste0("T", 1:8)], method.dat = "pearson", nrep = 500)


resboot


resboot$stab



# ----------
plot(resboot)

#plot(resboot, main = "", xlab = "", ylab = "", col.axis = "white", ell = list(lty = 1, col = "black", cex = 2,
#                                                                              label.conf = list(lable = TRUE, pos = 3, col = 1, cenx = 1.5)))


# -->
# These ellipses mark the 95% confidence regions of the points.
# They are reasonably compact, indicating that our MDS analysis has identified a configuration of points that are likely to lie at
# or close to the reported positions ... ?

