setwd("//media//kswada//MyFiles//R//perfume")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  perfume
#  - sensory data collected at Agrocampus of 98 consumers conducting categorization task using 12 luxury perfumes
#  - The participants were asked to divide the perfumes into groups according to their sensory similarities, and then to attribute a description to each of the groups.
# ------------------------------------------------------------------------------

perfume <- read.table("perfume.csv", header = TRUE, sep = ";", row.names = 1)

dim(perfume)

str(perfume)


perfume[,1:10]



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

library(FactoMineR)


graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(perfume, graph = TRUE)


# -->
# This command executes the MCA and produces the graph of variables, 
# the graph of individuals (featuring the individuals and the categories for the active and supplementary variables)
# as well as the graph of quantitative supplementary variables



# ----------
summary(res.mca)


# -->
# first 2 dimensions account 31.445% of the explained inertia.



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

plot(res.mca, invisible = c("var"), col.ind = "black", cex = 0.7)



# -->
# 1st principal component identifies the perfumes Shalimar, Aromatics Elixir, and Chanel 5 as being opposed to the other perfumes
# 2nd principal component opposes Angel, Lolita Lempicka, and , to a lesser extent, Cinema, with the other perfumes.

# The distancing of a small number of perfumes is related to the number of times that these perfumes were put into groups of their own:
# This is indeed the case for Shalimar, Chanel 5, and Angel, which were singled out by 24, 17, and 13 participants, respectively.

# The proximity of certain perfumes is related to the frequency with which they were assigned to the same group:
# this was the case for Aromatics Elixir, which was associated with Shalimar (Chanel 5) 42 times (and 51 times, respectively);
# and for Lolita Lempicka, which was associated with Angel 36 time.
# The two J'adore perfumes were grouped together 56 times and are therefore also close to one another.



# ------------------------------------------------------------------------------
# Cloud of variables
# ------------------------------------------------------------------------------


par(mfrow = c(1,1))

plot(res.mca, choix = c("var"), cex = 0.7)



# -->
# On the 1st component, participants 93 and 40 have high coordinates compared with participants 18 and 31.
# Participants 40 and 31 clearly sigled out the perfumes Shalimar, Aromatics Elixir, and Chanel 5, unlike participants 18 and 31.

# Participants 31 and 40 did indded single out Angel, and to a lesser extent Lolita Lempicka and Cinema, whereas participants 18 and 93 did not.



# ------------------------------------------------------------------------------
# Cloud of categories
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


# ----------
plot(res.mca, invisible = c("ind"), col.var = "black", cex = 0.7)



# -->
# The 1st component opposes the perfumes associated with the words "old", "strong", and the perfumes descrived as "flowery", "soft".
# The 2nd component opposes the perfumes associated with teh words "warm", "sweet", and "spicy" with the other perfumes.



