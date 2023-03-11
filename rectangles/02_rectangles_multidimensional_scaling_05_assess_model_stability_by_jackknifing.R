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
# Assess model stability by jackknifing
# ------------------------------------------------------------------------------

JK <- jackknife(res_mx)
# JK <- jackknife(res_mh)


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


