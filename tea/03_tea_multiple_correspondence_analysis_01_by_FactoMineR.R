setwd("//media//kswada//MyFiles//R//tea")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tea
#  - survey of 300 tea drinkers for tea-drinking behaviour (19 questions), the image they have of the product (12 questions) and 4 descriptive quesionts
# ------------------------------------------------------------------------------

tea <- read.table("tea.csv", header = TRUE, sep = ";")

dim(tea)

str(tea)


car::some(tea)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(tea, quanti.sup = 22, quali.sup = c(19:21, 23:36))



# -->
# This command executes the MCA and produces the graph of variables, 
# the graph of individuals (featuring the individuals and the categories for the active and supplementary variables)
# as well as the graph of quantitative supplementary variables/



# ----------
summary(res.mca)


# -->
# first 2 dimensions account only 17.99% of the explained inertia.



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

# plot(res.mca, invisible = c("var", "quali.sup"), cex = 0.7)



# -->
# As in most analyses of survey data, the cloud of individuals is made up of many points and our objective is to see if
# we can identify a specific shape, or groups of notable individuals.
# In the example, there are no notable groups of individuals: the cloud of points is a rather consistent shape.



# ----------
tea[c(200,262),]
tea[c(265,272),]

# 200 and 262:  at the negative extremity of the 1st principal component
# 265 and 273:  at the positive extremity of the 1st principal component


# -->
# Individual 265 and 273 both drink tea regularly and at all times of day
# Individual 200 and 262 only drink tea at home, either at breakfast or during the evening



# ------------------------------------------------------------------------------
# Cloud of variables
# ------------------------------------------------------------------------------


par(mfrow = c(1,1))

plot(res.mca, choix = c("var"), cex = 0.7)



# -->
# The variables type, format, and place of purchase are closely related to each of the first two components,
# altough it is unclear how (this appears in the representation of the categories).

# This graph is essentially useful to clear the way when faced with a large number of variables.



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

plot(res.mca, choix = "quanti.sup", cex = 0.7)

res.mca$quanti.sup$coord



# -->
# The quantitative supplementary variables are represented in the same way as in PCA:
# on a correlation circle using correlation coefficients between the variable and the principla components.

# This variable "age" is not well represented;
# However, the correlation with the second principal component (0.204) is significant due to the great number of individuals.
# Young perple are less likely to buy their tea in specialist shops.
# It can therefore also be said that older people are more likely to buy luxury loose tea in specialist shops.


