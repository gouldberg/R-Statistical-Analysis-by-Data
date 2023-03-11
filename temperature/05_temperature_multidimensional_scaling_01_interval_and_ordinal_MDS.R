setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category:  by dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

library(smacof)


# convert correlation to distance
cormat_v <- psych::cor2dist(cor(temperature[,1:12]))


# individual
cormat_i <- psych::cor2dist(cor(t(temperature[,1:12])))



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
# Note that month is positioned in circular way from January to December



