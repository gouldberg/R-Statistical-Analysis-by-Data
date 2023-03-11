setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)
str(Vietnam)


car::some(Vietnam)


# frequency form in dataframe to table
( vietnam.tab <- xtabs(Freq ~ sex + year + response, data = Vietnam) )

str(vietnam.tab)



# ----------
# stacking table
structable(response ~ sex + year, vietnam.tab)

viet3s <- as.matrix(structable(response ~ sex + year, vietnam.tab), sep = ":")

viet3s



# ------------------------------------------------------------------------------
# Principal inertia
# ------------------------------------------------------------------------------
# By default, ca() produces a 2-dimensional solution
( viet.ca <- ca::ca(viet3s) )


# 100% of the Pearson chi-square for association (= Principal inertias) is accounted for by 2 dimensions,
# with most of that attributed to the 1st dimension.
summary(viet.ca)



# The Pearson chi-square for this table is 366.36 = total inertia (the sum of eigenvalues(0.116415)) * n (3147)
chisq.test(viet3s)



# Complete, exact solution would have M = min((I-1)(J-1)) = 27 dimensions, and you could obtain this using the argument nd = 27
( viet.ca_comp <- ca::ca(viet3s, nd = 27) )



# ------------------------------------------------------------------------------
# Visualize pattern of associations by plotting CA scores in 2D
# ------------------------------------------------------------------------------
# Standard coordinates are transformed internally within the plot function according to the map argument: default to map = "symmetric",
# giving principal coordinates

# asp = 1:  2 axes are scaled so that the number of data units per inch are the same for both the horizontal and vertical axes.
graphics.off();  par(mfrow=c(1,1), asp = 1)
res <- plot(viet.ca)
segments(0, 0, res$cols[,1], res$cols[,2], col = "red", lwd = 2)
lines(res$rows[1:5,], col = "gray")
lines(res$rows[6:10,], col = "blue")

res



# -->
# It is important for interpretation of distances between points (and angles between vectors) for the axes to be equated.
# (this is symmetic map, so the distances between row points and distances between column points are meaningful)



# ------------------------------------------------------------------------------
# Visualize pattern of associations by mosaic plot
# ------------------------------------------------------------------------------

# order by Dim1 score
res.ca$rowcoord
tmp.order <- order(viet.ca$rowcoord[,1])
tmp.order

mosaic(viet3s[tmp.order,], gp = shading_Friendly2, suppress = 0, labeling = labeling_residuals, gp_args = list(interpolate = 1:4))





