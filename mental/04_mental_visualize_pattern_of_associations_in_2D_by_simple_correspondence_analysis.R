setwd("//media//kswada//MyFiles//R//mental")

packages <- c("dplyr", "vcd", "MASS", "ca", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Mental
# ------------------------------------------------------------------------------
data("Mental", package = "vcdExtra")

data <- Mental

data

tab <- xtabs(Freq ~ ses + mental, data = Mental)

tab



# ------------------------------------------------------------------------------
# Principal inertia
# ------------------------------------------------------------------------------
# By default, ca() produces a 2-dimensional solution
( mental.ca <- ca::ca(tab) )


# Nearly 99% of the Pearson chi-square for association (= Principal inertias) is accounted for by 2 dimensions,
# with most of that attributed to the 1st dimension.
summary(mental.ca)



# The Pearson chi-square for this table is 45.985 = total inertia (the sum of eigenvalues(0.277)) * n (1660)
chisq.test(tab)



# Complete, exact solution would have M = min((I-1)(J-1)) = 15 dimensions, and you could obtain this using the argument nd = 15
( mental.ca_comp <- ca::ca(tab, nd = 15) )



# ------------------------------------------------------------------------------
# Standard coordinates
#  - rescaling of the principal coordinates to unit inertia along each axis
#  - Standard coordinates differ from the principal coordinates simply by the absence of the scaling factors
#  - An asymmetric map shows one se of points in principal coordinates (say, the rows) in principal coordinates and the other set in standard coordinates
#
# Demonstrate orthogonality of std coordinates
# ------------------------------------------------------------------------------

# Standard coordinates Phi and Gamma
( Phi <- mental.ca$rowcoord )
( Gamma <- mental.ca$colcoord )


( Dr <- diag(mental.ca$rowmass) )
zapsmall(t(Phi) %*% Dr %*% Phi)

( Dc <- diag(mental.ca$colmass) )
zapsmall(t(Gamma) %*% Dc %*% Gamma)




# ------------------------------------------------------------------------------
# Visualize pattern of associations by plotting CA scores in 2D
# ------------------------------------------------------------------------------
# Standard coordinates are transformed internally within the plot function according to the map argument: default to map = "symmetric",
# giving principal coordinates

# asp = 1:  2 axes are scaled so that the number of data units per inch are the same for both the horizontal and vertical axes.
# It is useful to connect the row points and the column points by lines, to emphasie the pattern of these ordered variables
graphics.off();  par(mfrow=c(1,1), asp = 1)
res <- plot(mental.ca, ylim = c(-.2, .2))

lines(res$rows, col = "blue", lty = 3)
lines(res$cols, col = "red", lty = 4)


res



# -->
# It is important for interpretation of distances between points (and angles between vectors) for the axes to be equated.
# (this is symmetic map, so the distances between row points and distances between column points are meaningful)

# The plot of teh CA scores that diagnostic mental health categories are well-aligned with Dimension 1.
# The mental health scores are approximately equally spaced, except that the two intermediate categories are a bit closer on this dimension than the extremes.

# The SES categories are also aligned with Dimension 1, and approximately equally spaced, with the exception of the highest two SES categories,
# whose profiles are extremely similar, suggesting that these two categories could be collapsed.

# Because both row and column categories have the same pattern on Dimension 1,
# We may interpret the plot as showing that the profiles of both variables are ordered, and their relation can be explained as a positive association
# between high parenets' SES and higher mental health status of children.

# A mosaic display of these data would show a characteristic opposite corner pattern of association.
