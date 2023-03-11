setwd("//media//kswada//MyFiles//R//wish")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wish
# ------------------------------------------------------------------------------

data(wish, package = "smacof")


wish


car::some(wish)



# ------------------------------------------------------------------------------
# Assess model stability by jackknifing
# ------------------------------------------------------------------------------

JK <- jackknife(fit)
JK2 <- jackknife(res)


JK
JK2


# stability measure
JK$stab
JK2$stab



# ----------
par(mfrow=c(1,2))
plot(JK, col.p = "black", col.l = "black", lwd = 1.2)
plot(JK2, col.p = "black", col.l = "black", lwd = 1.2)



# -->
# Jackknife stability measure is hard to evaluate without benchmarks but plot(JK) exhibits the MDS configuration
# together with a star of lines attached to each point that gives you an impression of the stability of the MDS configuration under the various n - 1 conditions

# The end point of the stars show the respective point positions of the n - 1 jackknife configurations, fitted to each other,
# and subsequenctly connected to their centroid.



