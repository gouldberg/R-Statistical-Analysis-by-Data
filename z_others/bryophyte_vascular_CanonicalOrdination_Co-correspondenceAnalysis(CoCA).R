# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "cocorresp")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bryophyte,  vascular
#  - bryophytes and vascular plants in Carpathian spring meadows.
#  - The data set comprises 70 sites and the species of both communities that are present in at least 5 sites., i.e., 30 bryophyte and 123 vascular plant species.
# ------------------------------------------------------------------------------
# Extract the data (available in cocorresp)
data(bryophyte)
data(vascular)

dim(bryophyte)
dim(vascular)

car::some(bryophyte)
car::some(vascular)



# ------------------------------------------------------------------------------
# Predictive Co-correspondence Analysis (CoCA)
#  - Devoted to the simultaneous ordination of 2 communities sampled at the same sites.
#  - Its asymmetric form allows one to predict a community on the basis of the other.
#  - One could be interested in assessing the relationship between 2 living communities, e.g., invertebrates and plants.
#    This may be done for instance to verify if an invertebrate community is more related to the plant community or to other, e.g. physico-chemical constraints,
#    or else to predict one community that is difficult to sample by means of another, easier one.
#  - CoCA can be run in a symmetric way (Co-inertia analysis) does for other types of data, or in an asymmetric way, the one that interests us in this section.
#
#  - The first ordination axis of symmetric and asymmetric CoCA is obtained by weighted averages, where the species scores of one set are obtained as weighted averages
#   of the other set's site scores and the site scores are weighted averages of the species scores of their own set.
#  - Then in symmetric CoCA the next axes are extracted in ths same way, with the successive species scores uncorrelated with the previous ones.
#  - In asymmetric CoCA the successive axes are constructed such that the site scores derived from the predictor variables are uncorrelated with the previously derived site scores.
#
#  - The number of axes to interpret can be assessed either by cross-validation or by a permutation test.
# ------------------------------------------------------------------------------
# Co-correspondence analysis is computed using the function coca()
# The default option is method = "predictive" 
# The 2 canonical axes together explain 30.4% of the total variance of the bryophyte data.
( carp.pred <- coca(bryophyte ~ ., data = vascular) )


# Leave-one-out cross-validation
# More conservative cross-validasgtory fit is 24.8% of the first 2 axes.
# The cross-validatory fit culminates with 5 axes (28.3%), then it decreases because the predictive power decreases with more axes.
crossval(bryophyte, vascular)


# Permutation test  --> only 2 significant axes
( carp.perm <- permutest(carp.pred, permutations = 99) )


# Only two significant axes: refit
( carp.pred <- coca(bryophyte ~ ., data = vascular, n.axes = 2) )


# Extract the site scores and the species loadings used in the biplots
( carp.scores <- scores(carp.pred) )
load.bryo <- carp.pred$loadings$Y
load.plant <- carp.pred$loadings$X


# We have generated two plots.
# As in ter Braak and Schaffers (2004, Fig. 3), in both plots the site scores are derived from the vascular plants (carp.scores$sites$X)
# and the species scores are the "loadings with respect to normalized site scores"

# Printing options:
?plot.predcoca  

par(mfrow = c(1, 2))
plot(carp.pred, type = "none", main = "Bryophytes", xlim = c(-2, 3), ylim = c(-3, 2))
points(carp.scores$sites$X, pch = 16, cex = 0.5)
text(load.bryo, labels = rownames(load.bryo), cex = 0.7, col = "red") 
plot(carp.pred, type = "none", main = "Vascular plants", xlim = c(-2, 3), ylim = c(-3, 2))
points(carp.scores$sites$X, pch = 16, cex = 0.5)
text(load.plant, labels = rownames(load.plant), cex = 0.7, col = "blue") 


# Detach package cocorresp to avoid conflicts with ade4:
detach("package:cocorresp", unload = TRUE)
# If not sufficient:
unloadNamespace("cocorresp")
