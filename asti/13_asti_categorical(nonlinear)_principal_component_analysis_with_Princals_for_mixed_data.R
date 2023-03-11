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
  


# ----------
# For illustrative purposes we create sum scores for the remaining 3 scales
si <- rowSums(ASTI[, c(10, 19, 20, 21)])  # self-knowledge
pm <- rowSums(ASTI[, c(1, 5, 9, 22)])  # peace of mind
na <- rowSums(ASTI[, c(3, 6, 8, 12)])  # non-attachment

asti2 <- data.frame(stpg, si, pm, na)




# ------------------------------------------------------------------------------
# Princals on Mixed Input Data
# ------------------------------------------------------------------------------

library("Gifi")


# We apply linear transformations on the sum score variables, that is, we treat them as metric.
# Type = "D": place knots at the data points as needed for the ordinal transformation of the 13 items
# TYpe = "E": implies a linear transformation of the 3 sum scores
knotsord <- knotsGifi(asti2[, 1:13], type = "D")
knotslin <- knotsGifi(asti2[, 14:16], type = "E")

knotslist <- c(knotsord, knotslin)



# ----------
# For illustration purposes, we fit a 3D Princals.
# degrees = 1 is internally used for the linear transformations only: the ordinal transformations are not affected by this specification.
prordlin <- princals(asti2, knots = knotslist, degrees = 1, ndim = 3)

colvec <- c(rep("gray", 13), rep("coral", 3))

plot(prordlin, col.loadings = colvec, plot.dim = c(1,2))



# -->
# The sum scores for PM and SI are highly related to each other.
# The Short NA loadings vector suggests that NA has smaller loadings on the first 2 dimensions than PM and SI.
# Some of the PG items (11,14, and 17) appear to be strongly related to the 3 sum score variables.

