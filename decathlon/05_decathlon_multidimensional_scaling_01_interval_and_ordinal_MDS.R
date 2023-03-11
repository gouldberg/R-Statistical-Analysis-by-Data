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
# Multidimensional Scaling by each category:  by dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

library(smacof)


# convert correlation to distance
cormat_v <- psych::cor2dist(cor(decathlon[,1:10]))


# individual
cormat_i <- psych::cor2dist(cor(t(decathlon[,1:10])))



fitv_i <- mds(cormat_v, type = "interval")
fitv_o <- mds(cormat_v, type = "ordinal")

fiti_i <- mds(cormat_i, type = "interval")
fiti_o <- mds(cormat_i, type = "ordinal")


fitv_i
fitv_o
fiti_i
fiti_o



# ----------
par(mfrow = c(2, 2))
plot(fitv_i)
plot(fitv_o)
plot(fiti_i)
plot(fiti_o)



# -->
# 100m, 400m and 110 hurdle are closely positined
# Long.jump, Pole.vault, and javline are typical.

# Note taht ordinal and interval solutions are almost close


