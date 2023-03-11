setwd("//media//kswada//MyFiles//R//rep_vict")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Repeat victimization
# ------------------------------------------------------------------------------
data("RepVict", package = "vcd")

data <- RepVict

data

names(dimnames(data)) <- c("FirstVictimization", "SecondVictimization")



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
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Principal inertia
# ------------------------------------------------------------------------------
# By default, ca() produces a 2-dimensional solution
( victim.ca <- ca::ca(data) )


# For this table, 7 dimensions are required for an exact solution.
# Nearly 64.5% of the Pearson chi-square for association (= Principal inertias) is accounted for by 2 dimensions,
summary(victim.ca)


# Pearson's X^2
chisq.test(data)
( chisq <- sum(data) * sum(victim.ca$sv^2) )



# Complete, exact solution would have M = min((I-1)(J-1)) = 49 dimensions, and you could obtain this using the argument nd = 49
( victim.ca_comp <- ca::ca(data, nd = 49) )



# ------------------------------------------------------------------------------
# Visualize pattern of associations by plotting CA scores in 2D
# ------------------------------------------------------------------------------
# Standard coordinates are transformed internally within the plot function according to the map argument: default to map = "symmetric",
# giving principal coordinates

# asp = 1:  2 axes are scaled so that the number of data units per inch are the same for both the horizontal and vertical axes.

# Here, we want to emphasize the relation between the same crimes on the first and second occurrence.
# We lable each crime just once (using labels = c(2, 0)) and connect the 2 points for each crime by a line

graphics.off();  par(mfrow=c(1,1), asp = 1)
res <- plot(victim.ca, labels = c(2, 0))
segments(res$rows[,1], res$rows[,2], res$cols[,1], res$cols[,2])
legend("topleft", legend = c("First", "Second"), title = "Occurrence", col = c("blue", "red"), pch = 16:17, bg = "gray90")


res



# -->
# Most of points are extremely close for the first and second occurrence of a crime,
# indicating that the row profile for a crime is very similar to its corresponding column profile, with Rape and Pickpocket as exceptions.

# The first dimension appears to contract crimes against the person (right) with crimes against property (left),
# and it may be that the second dimension represents defree of violence associated with each crime.
# The latter interpretaion is consistent with the movement of Rape towards higher position and Pickpocket towards a lower one on this dimension.


