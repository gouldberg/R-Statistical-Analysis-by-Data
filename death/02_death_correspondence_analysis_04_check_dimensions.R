setwd("//media//kswada//MyFiles//R//death")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  death
# ------------------------------------------------------------------------------

death <- read.table("death.csv", header = TRUE, row.names = 1, sep = ";")

str(death)

dim(death)

head(death)


colnames(death) <- c("0_1", "1_4", "5_14", "15_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75_84", "85_94", "95+")



# ------------------------------------------------------------------------------
# First dimension
# ------------------------------------------------------------------------------

res.ca <- CA(death[1:65,], graph = TRUE)



# ----------
par(mfrow = c(1,1))
# plot(res.ca, invisible = "col")
plot(res.ca, invisible = "row")


# -->
# The first dimension separates newborns of 0 to 1 years from the other age groups



# ----------
par(mfrow = c(1,1))
# plot(res.ca, invisible = "col")
# plot(res.ca, invisible = "row")
plot(res.ca, cex = 0.8)



# -->
# It draws attention to the specific causes of mortality in this age group, that is to say,
# infant diseases which affect this age group exclusively, or quasi-exclusively (perinatal infection, SIDS, etc.)
# In this case, CA therefore highlights a specific phenomenon of a given category.



# ----------
# In CA, as the elements (rows and columns) do not have the same weight,
# one must consider the contributions before proposing an interpretation.
# They are expressed as percentages.

res.ca$col$contrib

res.ca$row$contrib


# -->
# The contributions confirm that the age group 0 - 1 year contributes (almost) entirely to the first dimension.
# At this age, therefore, causes of mortality are extremely specific.
# This result supports that relating to the inertia of this age group in the overall space (= 0.5262).


res.ca$row$contrib[rev(order(res.ca$row$contrib[,1])),1]



# -->
# The first dimension highlights the most prominent trait of the deviation from independence:
# causes of mortality specific to newborn babies.
# At this stage, two options are available:

# 1. As the specificity of this age range has been well established, this age group is removed from the analysos and the analysos conducted 
# a second time. In doing so, the range of our study is altered:
# We focus specifically on the population of individuals of more than one year old.
# One is often tempted by this method, which breaks a wide-ranging domain down into simple elements prior to studying them.

# 2. Continue the investigation of this CA. The orthogonality of the dimension ensures that as the uniqueness of the 0 to 1 age group has been
# expressed on the first dimension, it will not go on to "distort" the following axes.
# This is the tactic taht we chose to follow (and which, generally, we would recommend).



# ------------------------------------------------------------------------------
# Second and Third dimension
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

plot(res.ca, invisible = "row", axes = c(2,3), cex = 0.8)

plot(res.ca, invisible = "col", axes = c(2,3), cex = 0.8)



# -->
# The representation of the age groups identifies a Guttman effect (or horseshoe effect).
# Such an effect may appear in the case of ordered categorical variables when one axis confronts the smallest categories to the highest and another
# axis confronts those extreme categories to the ones in between.

# The second dimension (abscissa axis) confronts the youngest age groups with the eldest age groups where the third dimension confronts
# the extreme ages with the average age groups.

# Along dimension 2, the "adult" age groups (>= 15 years) are arranged according to their "natural" order.
# This indicates a regular increase of the mortality profile with age.
# 1. The different between the two curves of the parabola stem from the differences in sample sizes;
# the youngest categories are generally the rarest (and of course, in this case, this is not a criticisum, as we are talking about the number of deaths),
# and the origin of the dimensions is located in the clouds' centre of gravity (of both rows and columns).
# It is effectively found near to the categories with the greatest sample size
# (the average life expectancy is 70.98 years, which corresponds to the results in the grpah).
# Another approach to this difference in sample size between young and elderly age groups is that the latter are "automatically" closer to the average profile
# as they influence it the most.

# 2. Furthermre, the graph clearly shows that the upper age groups are closer to one another than those of younger adults.



# ----------
res.ca <- CA(death, row.sup = c(66:nrow(death)), ncp = Inf)



# Distances between age groups
round(dist(res.ca$col$coord), 3)


# -->
# This matrix illustrates the great distance between the 0 to 1 age group and the others
# Furthermore, it indicates that the distance between consevutivea age groups decrease steadily between 1 year and 54 years,
# after which it stabilises around a weak value.
# This is in line with our observation on plane 2-3 concerning the age groups above 15 years
# (for 1 to 4 and 5 to 15 years, other dimensions, including the first, are required to account for this specificity),



# ----------
# Contributions to the constructions of dimensions and the representation qualities for the different age groups
round(cbind(res.ca$col$contrib[,2:5], res.ca$col$cos2[,2:5]), 3)



# -->
# Along the second dimension, the age groups between 15 and 44 years account for a combined contribution of 64.211%,
# and is therefore the basis for the interpretation of the results.
# The contributions of these three age groups therefore support the coordinates (the three sample sizes are similar).
# The age group 15 - 24 is the most extreme example that we should focus our attention in order to illustrate the dimension.



# ----------
cbind(res.ca$row$contrib[,2], res.ca$row$cos[,2], res.ca$call$marge.row)[rev(order(res.ca$row$contrib[,2])),]

cbind(res.ca$row$contrib[,3], res.ca$row$cos[,3], res.ca$call$marge.row)[rev(order(res.ca$row$contrib[,3])),]



# -->
# Road accidents contribute the most to this dimension (41.05%) and have the coordinate with the highest value.
# This cause of death is characteristic of youn adults (high-value coordinate).
# This, along with its relatively high frequency means that youn adults account for an essential dimension
# (the second of the deviation from independence (high contribution).

# ....







