setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# Guttman-Browne circumplex mode for inter-correlations
#
#   - When testing theories about real data, forcing the points onto a perfect circle in MDS space may seem exaggerated formalism,
#     An approximate circle would be sufficient, but it is much harder to formulate this idea as a clear scaling target.
#     Moreover, a perfect circle is, by itself, rarely ever a meaninfgul structural theory.
#     It only becomes interesting if it is supplemented with additional notions such as a particular order of the points on the circle.
#   - Here in case of the data on personal values, the Theory on Universals in Values (Schwartx 1992) predicts such an order.
#     The theory also claims that the point order i structured into four subsets of opposite higher-order personal values.
#     This would split the circle into four arcs that lie in four different quadrants.
#
#   - If you have inter-correlations as data, circular scaling solutions with various additional constraints can be generated using the R package CircE.
#     This program implements the Guttman-Browne circumplex model for inter-correlations (Browne 1992).
#     It assumes that an observed correlation r(ij) corresponds to an angle between the vectors pointing to the points i and j on a unit circle.
#     The method does not accept order constraints, but they can be approximated to some extent by restricting the points to lie in certain sectors of the circle.
#
#   - CircE computes a circular configuration together with extensive output, including many fit indexes such as GIF, AGIF, RMSEA
#     that are used in structural equation modeling.
#     They test the hypothesis that the observed correlations match the correlations derived from the model.
# ------------------------------------------------------------------------------

# CircE is not available for R3.6 ...
library(CircE)



# CircE commands (with lots of default agruments): 
# lower and upper bounds of point angles
lower1 <- c(0,0,0,270,270,180,180,180,90,90)

upper1 <- c(90,90,90,360,360,270,270,270,180,180)

res <- CircE.BFGS(R, v.names = colnames(R), m = 1, N = 10, upper = upper1, lower = lower1, 
                  equal.com = FALSE, equal.ang = FALSE)


CircE.Plot(res, ef = 0.1)



# -->
# Here, the fit is highly significant, and the results are quite similar to previous analysis


