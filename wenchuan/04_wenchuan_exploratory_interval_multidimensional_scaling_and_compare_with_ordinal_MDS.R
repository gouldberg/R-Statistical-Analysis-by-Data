setwd("//media//kswada//MyFiles//R//wenchuan")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Wenchuan
# ------------------------------------------------------------------------------

data("Wenchuan", package = "MPsychoR")

str(Wenchuan)



# ------------------------------------------------------------------------------
# Exploratory Multidimensional Scaling by Interval MDS
#   - The symptoms were scored on 5-point rating scales. We can assume that there is some metric information in it and,
#     therefore fit an interval MDS.
#   - Naturally the stress value of the interval MDS will be worse than the ordinal stress.
#     However, if there are only slight differences, we may prefer the interval solution. The corresponding linear transformation is much
#     simpler to interpret, most likely more robust across multiple samples and from a statistical point of view, more parsimonious than the ordinal step function.
# ------------------------------------------------------------------------------

library(smacof)


fit.wenchuan3 <- mds(Wdelta, type = "interval")

fit.wenchuan3

summary(fit.wenchuan3)



# ------------------------------------------------------------------------------
# Compare by Shepard Diagram
# ------------------------------------------------------------------------------
par(mfrow = c(1,1))

plot(fit.wenchuan2, plot.type = "Shepard", main = "Shepard Diagram (Ordinal MDS)")

plot(fit.wenchuan3, plot.type = "Shepard", main = "Shepard Diagram (Interval MDS)")



# -->
# We see that, especially for smaller dissimilarities, the step function approximates the points much better than the interval fit.
# This explains the difference in the stress values.



# ------------------------------------------------------------------------------
# The contribution of each object to the total stress:  stress-per-point (SPP)
# ------------------------------------------------------------------------------

plot(fit.wenchuan3, plot.type = "stressplot", main = "Wenchuan Stress-per-Point")


# -->
# We see that "loss of interest in activities that you used to enjoy" (lossint)
# provides a fairly high stress contribution of 12.11%.
# This indicates that this symptom has a "special" relationship to the remaining symptoms in the solution.



# SPP information incorporated into a configuration plot (bubbleplot)
plot(fit.wenchuan3, plot.type = "bubbleplot", main = "Wenchuan Stress-per-Point")



# ------------------------------------------------------------------------------
# Assess stability of an MDS solution by bootstrap strategy
# ------------------------------------------------------------------------------

library(colorspace)

set.seed(123)

bootWen <- bootmds(fit.wenchuan2, data = Wenchuan, method.dat = "euclidean", nrep = 100)


# l: luminance value in the FCL color descriptions
cols <- rainbow_hcl(17, l = 60)

plot(bootWen, col = cols)



# -->
# Confidence ellipses are plotted around the origianl points.

# We have two options to deal with misfit:
#   - we keep the dimensionality and remove one (or more) objects with teh highest SPP values
#   - we increase the number of dimensions and examine the goodness of fit again.
#     we produce configuration plots for pairs of dimension 8such as D1 vs D2, D1 vs D3, D2 vs D3)
# Note that if, for interpretation purposes, the solution needs to be rotated, none of the goodness-of-fit statistics changes.



# ------------------------------------------------------------------------------
# Final configuration plot
#   - None of the two dimensions can be meaningfully interpreted.
#     Rather, we can establish 3 regions of symptoms
#     which correspond exactly to the three symptom clusters conceptualized in the DSM-IV (American Psychiatric Association, 1994):
#       - intrusive recollection (B-cluster),  avoidance/numbing (C-cluster), and arousal (D-cluster).
#   - In addition, we can interpret the position of each point to all the other points since all distances are Euclidean
#   - There is no need here to exclude symptoms with high SPP since the obtained stress value is fairly low and the interpretation is quite clear.
#
#   - From a more data-driven point of view, we can also compute a cluster analysis on the fitted MDS configuration distances (by using hierarchical clustering)
#     and incorporate the cluster membership information into the configuration plot by coloring the objects accordingly.
# ------------------------------------------------------------------------------

graphics.off()
colpal <- c(rainbow_hcl(3, c = 100))
pal <- palette(colpal)

memb <- c(1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3) 
plot(fit.wenchuan2, main = "Wenchuan Configuration", label.conf = list(label = FALSE), col = memb)

legend("topright", legend = c("intrusive recollection", "avoidance/numbing", "arousal"), col = colpal, pch = 20)
abline(-0.05, 0.2, col = "lightgray")
lines(c(0.1, -1),c(-0.03, 1), col = "lightgray")

palette(pal)
colpal <- c(rainbow_hcl(3, c = 100, l = 30))
pal <- palette(colpal)
text(fit.wenchuan2$conf[-7,], labels = rownames(fit.wenchuan2$conf)[-7], col = memb[-7], pos = 3, cex = 0.8)
text(fit.wenchuan2$conf[7,1], fit.wenchuan2$conf[7,2], labels = rownames(fit.wenchuan2$conf)[7], col = memb[7], pos = 1, cex = 0.8)
palette(pal)


