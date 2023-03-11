setwd("//media//kswada//MyFiles//R//jo")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  JO
# ------------------------------------------------------------------------------

data("JO", package = "FactoMineR")

str(JO)

dim(JO)


head(JO)



# ------------------------------------------------------------------------------
# dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

# correlation matrix --> conver to distance (dissimilarity)
distmat <- psych::cor2dist(cor(JO))


# but note that distances are clustered around 1.5 ...
hist(as.vector(distmat))



# ----------
# try other distance
distmat2 <- 1 - cor(JO)



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category:  by dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

library(smacof)


fit_1i <- mds(distmat, type = "interval")
fit_1o <- mds(distmat, type = "ordinal")
fit_2i <- mds(distmat2, type = "interval")
fit_2o <- mds(distmat2, type = "ordinal")

fit
fit2


# ----------
par(mfrow = c(2, 2))
plot(fit_1i, main = "config 1i")
plot(fit_1o, main = "config 1o")
plot(fit_2i, main = "config 2i")
plot(fit_2o, main = "config 2o")



# -->
# Surprisingly, we obtain circular configuration in type = "interval"
# Japan is slightly inside the circle by distmat2 and type = "ordinal"


