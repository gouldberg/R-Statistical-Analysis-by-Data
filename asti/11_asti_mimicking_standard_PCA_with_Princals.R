setwd("//media//kswada//MyFiles//R//asti")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ASTI
# ------------------------------------------------------------------------------

data("ASTI", package = "MPsychoR")


str(ASTI)



# ----------
st <- ASTI[, c(2, 4, 7, 13, 16, 24, 25)]
pg <- ASTI[, c(11, 14, 15, 17, 18, 23)]


stpg <- data.frame(st = st, pg = pg)
  


# ------------------------------------------------------------------------------
# Mimicking standard PCA (linear transformation) with Princals
# ------------------------------------------------------------------------------

library("Gifi")


# set up the knot structure of the underlying spline.
# simplest form of a spline is a line:  one knot at the minimum and one knot at the maximum value, and zero interior knots
# this setting is type = "E"
knotslin <- knotsGifi(stpg, type = "E")



# ----------
# The default number of dimensions p = 2
# degrees = 1 is a line
prlin <- princals(stpg, knots = knotslin, degrees = 1)

prlin



# ----------
# Check eigenvalues:  Standard PCA and linear Princals lead to the same results
(pcafit$sdev^2)[1:2]

prlin$evals[1:2]



# ----------
# Check loadings:  The differences in the loadings are due to different standardization
head(round(pcafit$rotation[,1:2], 3), 3)

head(round(prlin$loadings, 3), 3)


# The prcomp function standardized the loadings to SS = 1, whereas in princals they are standardized in a way that the SS correspond to the eigenvalues
apply(pcafit$rotation[,1:2], 2, function(pc) sum(pc^2))

apply(prlin$loadings, 2, function(pc) sum(pc^2))


prloads1 <- apply(prlin$loadings, 2, function(pc){ pc / sqrt(sum(pc^2)) })
head(round(prloads1, 3), 3)

head(round(pcafit$rotation[,1:2], 3), 3)


