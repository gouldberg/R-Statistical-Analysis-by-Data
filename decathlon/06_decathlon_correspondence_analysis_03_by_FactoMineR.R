setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Simple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

res.ca <- CA(decathlon[,1:13], quanti.sup = 11:12, quali.sup = 13)


# main results: inertia associated with each component, coordinates, contributions, 
# and representation qualities of the rows and columns
summary(res.ca)



# -->
# first 2 dimensions account 82.6% of the explained inertia.



# ------------------------------------------------------------------------------
# Percentage of inertia associated with a component
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
barplot(res.ca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.ca$eig)))



# -->
# first 2 dimensions are sufficient



# ------------------------------------------------------------------------------
# Contributions
# ------------------------------------------------------------------------------

# The larger scale of the dataset does not affect contribution, as this aspect is calculated for each component.


# The contributions of variables which contributed to the construction of dimension 1 in descending order.
round(res.ca$col$contrib[rev(order(res.ca$col$contrib[,1])), 1], digits = 5)


# ----------
# 2nd dimension
round(res.ca$col$contrib[rev(order(res.ca$col$contrib[,2])), 2], digits = 5)



# ----------
round(res.ca$row$contrib[rev(order(res.ca$row$contrib[,1])), 1], digits = 5)

round(res.ca$row$contrib[rev(order(res.ca$row$contrib[,2])), 2], digits = 5)



# ------------------------------------------------------------------------------
# Representation Quality
# ------------------------------------------------------------------------------

# Representation quality of variables for dimension 1 in descending order.
round(res.ca$col$cos2[rev(order(res.ca$col$cos2[,1])), 1], digits = 5)


# ----------
# 2nd dimension
round(res.ca$col$cos2[rev(order(res.ca$col$cos2[,2])), 2], digits = 5)



# ------------------------------------------------------------------------------
# Cloud of individuals
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(res.ca, invisible = c("col"), cex = 0.7)




# ------------------------------------------------------------------------------
# Cloud of variables
# ------------------------------------------------------------------------------


par(mfrow = c(1,1))

plot(res.ca, invisible = c("row"), cex = 0.7)



# ------------------------------------------------------------------------------
# Cloud of suppelementary variable
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


# ----------
plot(res.ca, choix = "quanti.sup", cex = 0.7)


