setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Assess model stability by jackknifing
# ------------------------------------------------------------------------------

JK_v <- jackknife(fitv_o)
JK_i <- jackknife(fiti_o)


JK_v
JK_i


# stability measure
JK_v$stab
JK_i$stab



# ----------
par(mfrow=c(1,2))
plot(JK_v, col.p = "black", col.l = "black", lwd = 1.2)
plot(JK_i, col.p = "black", col.l = "black", lwd = 1.2)



# -->
# Jackknife stability measure is hard to evaluate without benchmarks but plot(JK) exhibits the MDS configuration
# together with a star of lines attached to each point that gives you an impression of the stability of the MDS configuration under the various n - 1 conditions

# The end point of the stars show the respective point positions of the n - 1 jackknife configurations, fitted to each other,
# and subsequenctly connected to their centroid.



