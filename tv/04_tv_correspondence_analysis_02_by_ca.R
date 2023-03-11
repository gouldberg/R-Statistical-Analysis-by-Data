setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")


TV


dim(TV)


str(TV)



# ----------
TV2 <- margin.table(TV, c(1,3))

TV2




# ------------------------------------------------------------------------------
# Principal inertia
# ------------------------------------------------------------------------------

# By default, ca() produces a 2-dimensional solution
( tv.ca <- ca::ca(TV2) )


# 100% of the Pearson chi-square for association (= Principal inertias) is accounted for by 2 dimensions,
# with most of that attributed to the 1st dimension.
summary(tv.ca)



# The Pearson chi-square for this table is 3476.9 = total inertia (the sum of eigenvalues(0.092446)) * n (37610)
chisq.test(TV2)



# Complete, exact solution would have M = min((I-1)(J-1)) = 8 dimensions, and you could obtain this using the argument nd = 8
( tv.ca_comp <- ca::ca(TV2, nd = 8) )



# ------------------------------------------------------------------------------
# Visualize pattern of associations by plotting CA scores in 2D
# ------------------------------------------------------------------------------
# Standard coordinates are transformed internally within the plot function according to the map argument: default to map = "symmetric",
# giving principal coordinates

# asp = 1:  2 axes are scaled so that the number of data units per inch are the same for both the horizontal and vertical axes.
graphics.off();  par(mfrow=c(1,1), asp = 1)
res <- plot(tv.ca)
segments(0, 0, res$cols[,1], res$cols[,2], col = "red", lwd = 2)

res



# -->
# It is important for interpretation of distances between points (and angles between vectors) for the axes to be equated.
# (this is symmetic map, so the distances between row points and distances between column points are meaningful)

# The dominant dimension separates viewing on Thursday, with the largest share of viewers watching NBC, from the other weekdays.
# The second dimension in the CA plot separates CBS, with its greatest proportion of viewers on Monday, from ABC, with greater viewership on Wednesday and Friday.



# ------------------------------------------------------------------------------
# Visualize pattern of associations by mosaic plot
# ------------------------------------------------------------------------------
# order weekdays by Dim1 score
tv.ca$rowcoord
days.order <- order(tv.ca$rowcoord[,1])
days.order

mosaic(t(TV2[days.order,]), gp = shading_Friendly2, suppress = 0, labeling = labeling_residuals)


# -->
# In the mosaic plot, Thursday stands out as the only day with a higher than expected frequency for NBC, and this is the largest residual in the entire table




