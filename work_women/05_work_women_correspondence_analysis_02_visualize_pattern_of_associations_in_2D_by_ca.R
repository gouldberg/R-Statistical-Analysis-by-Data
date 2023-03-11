setwd("//media//kswada//MyFiles//R//work_women")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  work_women
# ------------------------------------------------------------------------------

work <- read.table("work_women.csv", header = TRUE, row.names = 1, sep = ";")

str(work)

dim(work)


colnames(work) <- c("stay.at.home", "part.time.work", "full.time.work", "totally.agree", "mostly.agree", "mostly.disagree", "totally.disagree")

names(work)

work



# ----------
# we divide into two tables
( work1 <- work[,1:3] )

( work2 <- work[,4:7] )



# ------------------------------------------------------------------------------
# Simple Correspondence Analysis
#  - Correspondense analysis is designed to show how the data deviate from expectation when the row and column variables are independent.
#    The sieve, association, and the mosaic plots depict every cell in the table, and for large tables it may be difficult to see patterns.
#    Correspondense analysis shows only row and column categories as points in the two (or three) dimensions that account for the greatest proportion of deviation from independence.
#
#  - The weighted average of the squared principal coordinates for the rows or columns on a principal axis equals the squared singular value for that axis,
#    whereas the weighted average of the squared standard coordinates equals 1.
#
#  - Reciprocal averages:  CA assigns scores to the row and column categories such that the column scores are proportional to the weighted averages of the row scores, and vice-versa.
#
#  - R software for correspondence analysis
#     - MASS::corresp():  the plot method calls biplot() for a 2 factor solution, using a symmetric biplot factorization that scales the row and column points
#       by the square roots of the singular values.  This handles matrices, data frames, and xtabs objects, but not table objects.
#
#     - MASS::mca():  multiple correspondense analysis
#
#     - ca::ca():  provides 2D plots via the plot.ca() method and interactive (rgl) 3D plots via plot3d.ca().
#       This package is the most comprehensive in terms of plotting options for various coordinate types, plotting supplementary points, and other features.
#       This package also accepts two-way tables, matrices, data frames, xtabs object.
#       Default common joint display is symmetric map (both sets of coordinates are scaled with the same weights for each axis)
#
#     - ca::mjca():  multiple and joint correspondense analysis of higher-way tables
#
#     - FactoMineR::CA() (Husson et al., 2015):  provides a wide variety of measures for the quality of the CA representation and many options for graphical display
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Principal inertia
# ------------------------------------------------------------------------------

# By default, ca() produces a 2-dimensional solution
( work.ca <- ca::ca(as.matrix(work1)) )



# 100% of the Pearson chi-square for association (= Principal inertias) is accounted for by 2 dimensions,
# with most of that attributed to the 1st dimension.
summary(work.ca)



# The Pearson chi-square for this table is 233.43 = total inertia (the sum of eigenvalues(0.1354)) * n (1754)
chisq.test(as.matrix(work1))



# Complete, exact solution would have M = min((I-1)(J-1)) = 4 dimensions, and you could obtain this using the argument nd = 4
( work.ca_comp <- ca::ca(as.matrix(work1), nd = 4) )



# ----------
# principal inertia (eigen values)
work.ca$sv^2


# coefficient = 1 / e\sqrt(eigenvalues)
1 / work.ca$sv



# ------------------------------------------------------------------------------
# Row mass and Column mass
# ------------------------------------------------------------------------------

work.ca


# row mass is column profiles
work.ca$rowmass

round(addmargins(prop.table(as.matrix(ddc), margin = 2), margin = 1), 3)



# ----------
# column mass is row profiles
work.ca$colmass

round(addmargins(prop.table(as.matrix(ddr), margin = 1), margin = 2), 3)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

work.ca$sv^2



# ------------------------------------------------------------------------------
# Standard coordinates
#  - rescaling of the principal coordinates to unit inertia along each axis
#  - Standard coordinates differ from the principal coordinates simply by the absence of the scaling factors
#  - An asymmetric map shows one se of points in principal coordinates (say, the rows) in principal coordinates and the other set in standard coordinates
#
# Demonstrate orthogonality of std coordinates
# ------------------------------------------------------------------------------

# Standard coordinates Phi and Gamma
( Phi <- work.ca$rowcoord )
( Gamma <- work.ca$colcoord )


( Dr <- diag(work.ca$rowmass) )
zapsmall(t(Phi) %*% Dr %*% Phi)

( Dc <- diag(work.ca$colmass) )
zapsmall(t(Gamma) %*% Dc %*% Gamma)




# ------------------------------------------------------------------------------
# Visualize pattern of associations by plotting CA scores in 2D
# ------------------------------------------------------------------------------
# Standard coordinates are transformed internally within the plot function according to the map argument: default to map = "symmetric",
# giving principal coordinates

# asp = 1:  2 axes are scaled so that the number of data units per inch are the same for both the horizontal and vertical axes.
graphics.off();  par(mfrow=c(1,1), asp = 1)
res <- plot(work.ca)

res



# -->
# It is important for interpretation of distances between points (and angles between vectors) for the axes to be equated.
# (this is symmetic map, so the distances between row points and distances between column points are meaningful)


# It must be noted that the origin of the axes coincides with the average profile (= barycenter) of each of the two clouds.


# -->
# "Stay at home" is on the same side as "only the husband works".
# a category with which it is strongly related, and is on the opposite side from the other tow categories, wich which it is weakly associated.

# "Both parents work equally" is on the same side as "full-time work" and is at the opposite side to "stay at home"


