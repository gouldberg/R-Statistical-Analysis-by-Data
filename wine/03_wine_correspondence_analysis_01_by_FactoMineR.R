setwd("//media//kswada//MyFiles//R//wine")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wine
#   - 
# ------------------------------------------------------------------------------

wine <- read.table("wine.csv", header = TRUE, row.names = 1, sep = ";", check.names = FALSE)


# this produces error
str(wine)


# data("wine", package = "FactoMineR")

dim(wine)

head(wine)



# ------------------------------------------------------------------------------
# Simple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

res.ca <- CA(wine, col.sup = 11, row.sup = nrow(wine))


# main results: inertia associated with each component, coordinates, contributions, 
# and representation qualities of the rows and columns
# show 10 by default, nbelements = Inf shows all the rows and columns
# ncp:  number of dimensions printed
sum(res.ca$eig[,"eigenvalue"])
summary(res.ca, nbelements = Inf, ncp = 2)



# -->
# Total inertia = 1.5, and subsequently X^2 of 368.79 (= 245(=n) * 1.5)
# associated with a p-value equal to almost zero.
# This table has poor validity test conditions (in theory, at least 80% of the theoretical sample size must have a value higher than 5 and none of them should be nil.)
# However, the p-value is so low that the worth of CA for this data cannot be questioned.

# The intensity of the relationship, measured using Cramer's V, is rather strong = 0.409
# (where 1 corresponds to a mutual exclusivity between each wine and a group of words)



# ------------------------------------------------------------------------------
# Plot rows and columns separately
# if the number of rows and columns are large, this might help.
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

plot(res.ca, invisible = "col")

plot(res.ca, invisible = "row")


# -->
# As shown alsl by Multidimensional Scaling, 6C and 1S is positioned far too !!


# ----------
wine


# ------------------------------------------------------------------------------
# Number of dimensions to examine
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
barplot(res.ca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.ca$eig)))


# -->
# The sequence of eigenvalues shows tow dimensions with noticeably higher inertias than on the folllowing dimensions, which,
# when considered along with the accrued inertia percentage of 53.6%, leads us to focus our interpretation on the first plane.
# These two dimensions each have rather high inertia. (0.4355 and 0.3713)



# ----------
# the dimension 3 and 4

par(mfrow = c(1,2))
plot(res.ca, axes = 3:4, invisible = "col")
plot(res.ca, axes = 3:4, invisible = "row")


res.ca$row$coord[,c("Dim 3", "Dim 4")]
res.ca$col$coord[,c("Dim 3", "Dim 4")]



# -->
# Dimension 3 and 4 also separate the discus and the hammer from the walking events (20km and 50km)
# The javelin is a very different event than those of the hammer and the discus.



# ------------------------------------------------------------------------------
# Representing exact barycenters
# ------------------------------------------------------------------------------

coord.col <- sweep(res.ca$col$coord, 2, sqrt(res.ca$eig[,1]), FUN = "*")

coord.row <- sweep(res.ca$row$coord, 2, sqrt(res.ca$eig[,1]), FUN = "*")



# ----------
graphics.off()
par(mfrow = c(1,2))

plot(res.ca, invisible = "col")
points(coord.col, pch = 17, col = "red")
text(coord.col, rownames(coord.col), col = "red")

plot(res.ca, invisible = "row")
points(coord.row, pch = 20, col = "blue")
text(coord.row, rownames(coord.row), col = "blue")




# ------------------------------------------------------------------------------
# Coordinates, relative contributions (in %), and Representation, Quality for Each Category and Dimension
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
plot(res.ca)


summary(res.ca, nbelements = Inf)


# ----------
# contribution:
# the contribution of point i to the inertia of the dimension of rank s
# the atypical case of a dimension caused by only one or two points can thus be detected immediately



# ----------
# quality of points on a dimension:
# indicates how the deviation of category i from the average profile is expressed on the dimension of rank s.



# ----------
# Distance (squared) to Average Profile and Inertia (in Initial Spaces)
res.ca$row$inertia / res.ca$call$marge.row
res.ca$row$inertia

res.ca$col$inertia / res.ca$call$marge.col
res.ca$col$inertia


# -->
# Distances does not take the category's sample size into account.



# ------------------------------------------------------------------------------
# Contributions
#  - It must be noted that in CA, unlike PCA, the most extreme elements are not necessarily those which contributed the most of the construction
#    of the dimensions, as the weights differ from one element to another.
# ------------------------------------------------------------------------------

# The contributions of countries which contributed to the construction of dimension 1 in descending order.
round(res.ca$col$contrib[rev(order(res.ca$col$contrib[,1])), 1], digits = 5)


# ----------
# 2nd dimension
round(res.ca$col$contrib[rev(order(res.ca$col$contrib[,2])), 2], digits = 5)



# ------------------------------------------------------------------------------
# row and column margins
# ------------------------------------------------------------------------------

# 245 as total medals
tot <- 245

res.ca$call$marge.row

res.ca$call$marge.row * tot

res.ca$call$marge.col

res.ca$call$marge.col * tot




# ------------------------------------------------------------------------------
# Discoveries from the data
# ------------------------------------------------------------------------------

# 1. Aubuissieres Silex (6), characterised by sweet, cited 11 times for this wine.
# This is the only wine to contain more than trace level residual sugars.
# This unusual (altough authorised) characteristic for a dry wine, stands out, as it is only rarely cited for the other wines
# (7 times in total, but never more than twice for one wine),
# and accounts for over a third of the words associated with this wine.
# The graph highlights the wine's lack of character; although this term was only cited 3 times for this wine,
# we have classed it in second place (among other things, this characteristic is really a lack of a characteristic and is therefore less evocaive).

# 2. Aubuissieres Marigny (7) + Fontainerie Coteaux (10).
# THese two wines were mainly characterised by the terms oak, woody, which were each cited 7 and 5 times,
# respectively, whereas the word was only used 3 times elsewhere.
# This description can, of course, be linked to the fact that these two wines are the only two to have been cask aged.
# According to this plane, foreign flavour best characterises these wines, but we chose to place it second due to the low
# frequency of this term (4), even if it was cited for these two wines alone.
# It should also be noted that the effect of ageing wine in casks does not only lead to positive characteristics.

# 3. The finve Touraine wines (Sauvignon; 1-5).
# Characterising these wines was more difficult. The terms "lots of character" "fresh", "delivate", "discrete", and "citrus" were
# cited for these wines, which seems to fit with the traditional image of a Sauvignon wine,
# according to which this vice yields fresh, flavoursome wines.
# We can also add two more marginal characteristics: "musty" (and "little character", respectively),
# cited 8 times (4 times, respectively), and which are never used to describe the Sauvignon wines.



# Once these 3 poles are established,
# we can go on to qualify the dimensions.
# The first distinguishes the Sauvignons from the Chenin wines based on freshness and flavour.
# The second opposes the cask-aged Chenin wines (with an oak flavor) with that containing residual sugar (with a sweet flavour).
# Having determined these outlines, the term "lack of character", which was only used for wines 6 and 8, seems to appear in the right place,
# i.e., far from the wines which could be described as flavoursome,
# whether the flavour be due to the Sauvignon vines or from being aged in oak casks.

# Finally, this plane offers an image of the Touraine white wines,
# according to which the Sauvignons are similar to one another and the Chenins are more varied.
# This could therefore be understood in a number of different, noncontradictory ways:
#  - There is only one way of producing Sauvigon whereas there are numerous ways of making Chenin.
#  - Viticulturists "work harder" to produce Chenin, the noble Touraine vine,
#    by testing many different techniques in order to obtain the best flavour.

# Dimension 3 and 4:
# Dimension 3 confronts wines 1 and 4 with the words "dry delicate" and "vigorous".
# Aside from the fact that they both have small sample sizes, they do not suggest that they require interpretation.
# Dimension 4 underlines wine number 5, which associates the words "lively" and "slightly acidic".
# Again, this association can be observed in the data (just barely). but it also deals with small sample sizes and does not imply
# anything specific.
# (on the contrary, "lively" and "slightly acidic" tend to oppose one another).



