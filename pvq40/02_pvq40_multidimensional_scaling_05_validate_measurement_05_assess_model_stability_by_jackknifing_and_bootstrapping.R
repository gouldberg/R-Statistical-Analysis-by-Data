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
# Assess model stability by jackknifing
# ------------------------------------------------------------------------------

JK <- jackknife(result)


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

# We here see that this MDS configuration is almost not affected at all whne single points are left out.



# ------------------------------------------------------------------------------
# Assess model stability by bootstrapping
#   - This requires a person-by-variable matrix from which the proximities are derived.
# ------------------------------------------------------------------------------

set.seed(123)

# resboot <- bootmds(result, data = PVQ40agg, method.dat = "pearson", nrep = 500)
resboot <- bootmds(result, data = R, method.dat = "pearson", nrep = 500)


resboot


resboot$stab



# ----------
plot(resboot)

#plot(resboot, main = "", xlab = "", ylab = "", col.axis = "white", ell = list(lty = 1, col = "black", cex = 2,
#                                                                              label.conf = list(lable = TRUE, pos = 3, col = 1, cenx = 1.5)))


# -->
# These ellipses mark the 95% confidence regions of the points.
# They are reasonably compact, indicating that our ordinal MDS analysis has identified a configuration of points that are likely to lie at
# or close to the reported positions.

