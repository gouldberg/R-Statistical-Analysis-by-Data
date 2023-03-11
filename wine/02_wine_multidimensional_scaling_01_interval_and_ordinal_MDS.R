setwd("//media//kswada//MyFiles//R//wine")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wine
#   - 
# ------------------------------------------------------------------------------

wine <- read.table("wine.csv", header = TRUE, row.names = 1, sep = ";", check.names = FALSE)


# this produces error
str(wine)


# data("wine", package = "FactoMineR")

dim(wine)

head(wine)



# ------------------------------------------------------------------------------
# dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

# correlation matrix --> conver to distance (dissimilarity)

# focus on column (wine)
distmat <- psych::cor2dist(cor(wine[1:(nrow(wine)-1), 1:(ncol(wine)-1)]))
# focus on column (sensory words)
# distmat <- psych::cor2dist(cor(t(wine[1:(nrow(wine)-1), 1:(ncol(wine)-1)])))


# but note that distances are clustered around 1.5 ...
hist(as.vector(distmat))



# ----------
# try other distance
# focus on column (wine)
distmat2 <- 1 - cor(wine[1:(nrow(wine)-1), 1:(ncol(wine)-1)])
# focus on column (sensory words)
# distmat2 <- 1 - cor(t(wine[1:(nrow(wine)-1), 1:(ncol(wine)-1)]))



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category:  by dissimilarity matrix calculated from correlations
# ------------------------------------------------------------------------------

library(smacof)


fit_1i <- mds(distmat, type = "interval")
fit_1o <- mds(distmat, type = "ordinal")
fit_2i <- mds(distmat2, type = "interval")
fit_2o <- mds(distmat2, type = "ordinal")



# ----------
par(mfrow = c(2, 2))
plot(fit_1i, main = "config 1i")
plot(fit_1o, main = "config 1o")
plot(fit_2i, main = "config 2i")
plot(fit_2o, main = "config 2o")



# -->
# It seems that 6C and 1S is positined far most.


