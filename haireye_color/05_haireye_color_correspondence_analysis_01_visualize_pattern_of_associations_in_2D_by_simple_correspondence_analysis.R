setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "ca")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(data, 1:2) )



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
( haireye.ca <- ca::ca(haireye) )


# Nearly 99% of the Pearson chi-square for association (= Principal inertias) is accounted for by 2 dimensions,
# with most of that attributed to the 1st dimension.
summary(haireye.ca)



# The Pearson chi-square for this table is 138.29 = total inertia (the sum of eigenvalues(0.2336)) * n (592)
chisq.test(haireye)



# Complete, exact solution would have M = min((I-1)(J-1)) = 3 dimensions, and you could obtain this using the argument nd = 3
( haireye.ca_comp <- ca::ca(haireye, nd = 3) )



# ------------------------------------------------------------------------------
# Standard coordinates
#  - rescaling of the principal coordinates to unit inertia along each axis
#  - Standard coordinates differ from the principal coordinates simply by the absence of the scaling factors
#  - An asymmetric map shows one se of points in principal coordinates (say, the rows) in principal coordinates and the other set in standard coordinates
#
# Demonstrate orthogonality of std coordinates
# ------------------------------------------------------------------------------

# Standard coordinates Phi and Gamma
( Phi <- haireye.ca$rowcoord )
( Gamma <- haireye.ca$colcoord )


( Dr <- diag(haireye.ca$rowmass) )
zapsmall(t(Phi) %*% Dr %*% Phi)

( Dc <- diag(haireye.ca$colmass) )
zapsmall(t(Gamma) %*% Dc %*% Gamma)




# ------------------------------------------------------------------------------
# Visualize pattern of associations by plotting CA scores in 2D
# ------------------------------------------------------------------------------
# Standard coordinates are transformed internally within the plot function according to the map argument: default to map = "symmetric",
# giving principal coordinates

# asp = 1:  2 axes are scaled so that the number of data units per inch are the same for both the horizontal and vertical axes.
graphics.off();  par(mfrow=c(1,1), asp = 1)
res <- plot(haireye.ca)

res



# -->
# It is important for interpretation of distances between points (and angles between vectors) for the axes to be equated.
# (this is symmetic map, so the distances between row points and distances between column points are meaningful)

# Dimension 1:  dark (left) vs. light (right).  Along the dimension 1, the eye colors could be considered roughly equally spaced, 
#   but for the hair colors, Blond is quite different in terms of its frequency profile
# Dimension 2:  largely contrasts red hair and green eyes

