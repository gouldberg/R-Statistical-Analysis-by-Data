# setwd("//media//kswada//MyFiles//R//orange")
setwd("C:\\Users\\kouse\\Desktop\\R\\audiometric")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  audiometric
#   - see "A User's Guide to Principal Components" p.106
#   - the values are threshold measurements calibrated in units referred to as 'decibel loss' in comparison
#     to a reference standard for that particular instrument.
#     Observations are obtained, one ear at a time, for a number of frequencies
# ------------------------------------------------------------------------------

audiometric <- read.table("audiometric.txt", header=TRUE, sep="\t")

str(audiometric)

dim(audiometric)


audiometric



# ------------------------------------------------------------------------------
# Multidimensional Scaling Biplot
# ------------------------------------------------------------------------------

library(smacof)


dat_mat <- psych::cor2dist(cor(t(audiometric[,2:9])))


fit_mds <- mds(dat_mat, type = "interval")



fit_mds


# -->
# stress-1 value is only 0.153 (high...)


summary(fit_mds)


# ----------
graphics.off()
par(mfrow=1,1)
plot(fit_mds)



