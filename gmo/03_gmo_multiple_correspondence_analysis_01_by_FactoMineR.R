setwd("//media//kswada//MyFiles//R//gmo")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gmo
# ------------------------------------------------------------------------------

gmo <- read.table("gmo.csv", header = TRUE, sep = ";", dec = ".")

dim(gmo)

str(gmo)

car::some(gmo)


# ----------
levels(gmo$Position.Al.H)[4] <- levels(gmo$Position.Al.H)[1]

levels(gmo$Position.Culture) <- c("Favourable", "Somewhat Against", "Totally opposed", "Favourable")



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

# By design, the illustrative variables do not contribute to the construction of the principal components and likewise for their associated categories.
# Therefore, it is unnecessary to group together the rarer categories.

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(tea, quanti.sup = 22, quali.sup = c(19:21, 23:36))



# -->
# This command executes the MCA and produces the graph of variables, 
# the graph of individuals (featuring the individuals and the categories for the active and supplementary variables)
# as well as the graph of quantitative supplementary variables



# ----------
summary(res.mca)


# -->
# first 2 dimensions account 33.46% of the explained inertia.



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR with ventilation
#   - It is also possible to group the categories automatically using ventilation.
#     This distribution is random or accounts for the order of the categories within a variable if the variable is ordered.
#   - The concept of ventilation is to randomly attribute those individuals associated with rare categories to other categories.
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(2,2))

# level.ventil designates the threshold below which a category is ventilated.
# here if a category is selected by less than 5% of individuals, they are distributed among the existing categories
res.mca <- MCA(gmo, ncp = 5, quali.sup = 17:21, level.ventil = 0.05)



# ----------
summary(res.mca)


# -->
# first 2 dimensions account 33.46% of the explained inertia:  do not change



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

plot(res.mca, invisible = c("var", "quali.sup"), label = "none", cex = 0.7)



# -->
# The cloud of individuals is shaped like a parabola:  this is known as the Guttman effect (or horseshoe effect).
# This effect illustrates the redundancy of the active variables. or in other words, a cloud of individuals that is highly structured according to the 1st principal component.




# ------------------------------------------------------------------------------
# Cloud of variables
# ------------------------------------------------------------------------------


par(mfrow = c(1,1))

plot(res.mca, choix = c("var"), cex = 0.7)



# -->
# This graph is essentially useful to clear the way when faced with a large number of variables.



# ------------------------------------------------------------------------------
# Cloud of categories
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


# ----------
plot(res.mca, invisible = c("ind", "quali.sup"), cex = 0.7, label = "none")
plot(res.mca, invisible = c("ind", "quali.sup"), cex = 0.7)




# -->
# In the same way as the cloud of individuals, the shape of the cloud of categories resembles a parabola, and thus still represents a Guttman effect.

# On the positive side of the 1st principal component, we can observe those people who feel implicated by the debate surrounding GMO and
# who are somewhat against their use (through the categories they chose).

# On the negative side of the same principal component, we can see those people who do not feel implicated by the debate surrounding GMO and 
# who are in favour of theri use.

# Along the second principal component, we can also observe those people with less distinct opinions who feel somewhat implicated
# by the debate surrounding GMO and who are somewhat against their use.



# ------------------------------------------------------------------------------
# Supplementary Elements
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(res.mca, col.quali.sup = "blue", invisible = c("quanti.sup", "ind", "var"), cex = 0.7)

res.mca$quali.sup$coord



# -->
# This representation of supplementary variables is particularly remarkable as it provides two types of information.
# It reveals a strong structure for both of the variables profession and identification with a political movement,
# and second, it fails to identify any aprticular structure with the variables of age, sex, or profession in relation to agriculture,
# the food industry, and the pharmaceutical industry.

# The categories "senior management", "unemployed" and "retired" are in opposition to the categories "technician" and "manual labourer" 
# to "civil seervant" between the two groups.
# Similarly, the category "right" is opposed to the categories "green" and "extreme left", to, in the middle, "left".



