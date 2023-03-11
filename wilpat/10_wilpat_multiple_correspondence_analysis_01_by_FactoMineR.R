setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
# Let us pick six items: gay marriage, sexual freedom, gay adoption, gender quotas, affirmative action, and legalized marijuana
# and country variable (Hungary, the USA, and India)
WP6 <- WilPat[, c(32, 38, 41, 44, 45, 46, 47)]



# ----------
# Convert numeric to factor
WP6 <- rapply(WP6, f = as.factor, classes = "numeric", how = "replace")


str(WP6)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))


res.mca <- MCA(WP6)
# res.mca <- MCA(WP6, quali.sup = 7)



# -->
# This command executes the MCA and produces the graph of variables, 
# the graph of individuals (featuring the individuals and the categories for the active and supplementary variables)
# as well as the graph of quantitative supplementary variables/



# ----------
summary(res.mca)


# -->
# first 2 dimensions account only 33.7% of the explained inertia.  (this is same as homals)



# ------------------------------------------------------------------------------
# Percentage of inertia associated with a component
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
barplot(res.mca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.mca$eig)))



# -->
# In MCA, the percentages of inertia associated with the first components are generally much lower than in PCA.
# This is because, in PCA, only the linear relationships are studied: one single component should be sufficient to represent all of the variables
# if they are highly correlated.
# In MCA, we are studying much more general relationships and at least min(Kj-1, Kl-1) dimensions are required in order to represent the relationship between two variables,
# each of which has KJ and Kl categories, respectively.



# ------------------------------------------------------------------------------
# Contributions
# ------------------------------------------------------------------------------

# The larger scale of the dataset does not affect contribution, as this aspect is calculated for each component.
# It should be noted that the contribution of a categorical variable to a given component can be calculated by adding the contributions to these categories.


# The contributions of variables which contributed to the construction of dimension 1 in descending order.
round(res.mca$var$contrib[rev(order(res.mca$var$contrib[,1])), 1], digits = 5)


# ----------
# 2nd dimension
round(res.mca$var$contrib[rev(order(res.mca$var$contrib[,2])), 2], digits = 5)



# ------------------------------------------------------------------------------
# Representation Quality
# ------------------------------------------------------------------------------

# Due to the scale of the dataset, representation quality on a given plane is often much weaker compared to the representation qualities obtained in CA (or PCA)

# Representation quality of variables for dimension 1 in descending order.
round(res.mca$var$cos2[rev(order(res.mca$var$cos2[,1])), 1], digits = 5)


# ----------
# 2nd dimension
round(res.mca$var$cos2[rev(order(res.mca$var$cos2[,2])), 2], digits = 5)



# ------------------------------------------------------------------------------
# Cloud of individuals
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

# plot(res.mca, invisible = c("var", "quali.sup"), cex = 0.7, label = "none")
plot(res.mca, invisible = c("var", "quali.sup"), cex = 0.7)



# -->
# As in most analyses of survey data, the cloud of individuals is made up of many points and our objective is to see if
# we can identify a specific shape, or groups of notable individuals.
# In the example, there are no notable groups of individuals: the cloud of points is a rather consistent shape.



# ----------
# largest coordinate in second dimension  --> all "don't know" (2) answer
WP6[455,]




# ------------------------------------------------------------------------------
# Cloud of variables
# ------------------------------------------------------------------------------


par(mfrow = c(1,1))

plot(res.mca, choix = c("var"), cex = 0.7)




# ------------------------------------------------------------------------------
# Cloud of categories
# ------------------------------------------------------------------------------


par(mfrow = c(1,1))

plot(res.mca, invisible = c("ind", "quali.sup", "quanti.sup"), cex = 0.7)



# -->
# As with the supplementary categorical variables in PCA, the categories can be represented at the varycentre of the individuals
# with those categories.
# This representation is optimized as it correspoinds to the representation obtained by maximising the inertia of the cloud of categories
# on a sequence of orthogonal axes by homothetic transformations.


# -->
# The 1st component opposes the categories teamroon, supermarket+specialist shop, teabag+loose, bar, restaurant, work with the categories
# not.with.friends, not.work, not.restaurant, not.home.
# This 1st component thus opposes regular tea drinkers with those who drink tea only occasionally.

# The second component distinguishes the categories specialist shop, loose, luxury and, to a lesser extent, 
# green and after dinner, from all of the other categories.



# ------------------------------------------------------------------------------
# Supplementary Elements
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(res.mca, invisible = c("ind", "var"), cex = 0.7)

res.mca$quali.sup$coord


