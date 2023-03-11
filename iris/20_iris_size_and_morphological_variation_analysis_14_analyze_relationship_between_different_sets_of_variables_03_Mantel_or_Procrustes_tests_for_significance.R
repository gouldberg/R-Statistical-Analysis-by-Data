setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iris
# ------------------------------------------------------------------------------
data("iris")


str(iris)

car::some(iris)



# ------------------------------------------------------------------------------
# Analyzing relationships between different sets of variables
# Mantel test for significance
#
#   - Mantel test aims to determine whether dissimilarities or covariation matrix are similar.
#     For instance, the Mantel test is used to determine whether there is a relationship between geographical distances versus morphological Euclidean distances,
#     or ecological data versus morphological data.
#   - One can use the Mantel test to investigate whether the position of observations is similar between shape spaces or from spaces.
# ------------------------------------------------------------------------------

tmp1 <- iris %>% filter(Species == "setosa")
tmp2 <- iris %>% filter(Species == "versicolor")
tmp3 <- iris %>% filter(Species == "virginica")



library(ape)

unlist(mantel.test(as.matrix(dist(tmp1[,1:2])), as.matrix(dist(tmp1[,3:4]))))

unlist(mantel.test(as.matrix(dist(tmp2[,1:2])), as.matrix(dist(tmp2[,3:4]))))

unlist(mantel.test(as.matrix(dist(tmp3[,1:2])), as.matrix(dist(tmp3[,3:4]))))



# ------------------------------------------------------------------------------
# Analyzing relationships between different sets of variables
# Procrustes test
#
#   - We can compare the relative positions of Iris obseervations in the space defined by original variables between two sets.
#   - vegan::protest() performs scaling, rotations, translations and eventually reflections for finding the best match betwen configurations.
#     Since the test allos reflection, it looks at the geometry of the space rather than at functional relationships between its components.
# ------------------------------------------------------------------------------

library(vegan)

( prot1 <- protest(tmp1[,1:2], tmp1[,3:4]) )

( prot2 <- protest(tmp2[,1:2], tmp2[,3:4]) )

( prot3 <- protest(tmp3[,1:2], tmp3[,3:4]) )



# ----------
# Examining the fit
plot(prot1)

plot(prot2)

plot(prot3)



# -->
# Neither Procrustes nor Mantel tests find significant similarity between distances obtained from the sepal and petal measurements for the first species of Iris.
# The relationships between morphologies are nonetheless significant for the two other species.


# A last alternative test could be used to compute a X^2 statistic measure of association between the two covariance matrices;
# however, the dgrees of freedom are uncertain because of the correlative nature of the data.





