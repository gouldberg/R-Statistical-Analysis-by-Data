setwd("//media//kswada//MyFiles//R//jo")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  JO
# ------------------------------------------------------------------------------

data("JO", package = "FactoMineR")

str(JO)

dim(JO)


head(JO)



# ------------------------------------------------------------------------------
# Simple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
res.ca <- CA(JO)


# main results: inertia associated with each component, coordinates, contributions, 
# and representation qualities of the rows and columns
# show 10 by default, nbelements = Inf shows all the rows and columns
summary(res.ca, nbelements = Inf)


# comparison with ca()



# -->
# X^2 has a value of 2122 and is associated with a very low p-value.
# Here, however, the total sample size (5 * 5 * 24 = 360 medals) is extremely small
# in terms of the number of cells in the table (24 * 58 = 1392).
# We are thus far froma the conditions of test validity (even the most "relaxed" of which consider 80%
# of the theoretical sample size to be greater than 5, and the others to be greater than 1)
# and the p-value here can be considered for information only.
# However, the p-value is here so small that the significance of the deviation of this table from independence cannot be questioned.



# ------------------------------------------------------------------------------
# Plot rows and columns separately
# if the number of rows and columns are large, this might help.
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

plot(res.ca, invisible = "col")

plot(res.ca, invisible = "row")



# -->
# Regarding to the 1st dimension, the results here are rather astonishing,
# as all of the events involving long-distance races are separated from the other events on the 1st dimension.
# Among other things, there is a gradient of the different events going from 10000 m to 800 m.
# All of the events, without exception, are sorted from the longest distance to the shortest.
# This means that the results of the 10000m are more noteworthy than those of the other endurance events.
# Nonetheless, the marathon is much closer to the centre of the graph than expected.
# This is due to the fact that it is not the same kind of endurance event as the others.

# Here we find African countries that specialise in endurance events and New Zealand.
# N.B. (New Zealand only won one medal, in the 1500m, which is why it has such an extreme coordinate)

res.ca$col$coord["nzl",c("Dim 1", "Dim 2")]

JO[,"nzl"]
JO[,"jpn"]



# The second dimension separates sprinting evens from the discus, the hammer, and walking (20km and 50km).
# Again, here there is a gradient between the sprinting events.
# The relay events are less extreme than the individual events.
# Here, we can clearly observe that the 400m is a sprinting event,
# whereas the 800m is an endurance event.



# ------------------------------------------------------------------------------
# Number of dimensions to examine
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
barplot(res.ca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.ca$eig)))


# -->
# eigenvalue (i.e., the inertia or explained variance) associated with each dimension,
# the percentage of inertia it represents in the analysis,
# and the sum of these percentages.
# The first 2 dimensions express 24.40% of the total inertia.
# It may be interesting to consider the next dimensions, which also express a high percentage of the total inertia.



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

JO[,c("ken", "eth", "mar", "usa")]


# -->
# Ethiopia, Kenia, and Morocco account for 65% of the construcion of the 1st dimension
# These countries won a great number of medals in these events.


# ----------
# 2nd dimension
round(res.ca$col$contrib[rev(order(res.ca$col$contrib[,2])), 2], digits = 5)

JO[,c("usa", "ltu", "blr", "hun")]



# ------------------------------------------------------------------------------
# row and column margins
# ------------------------------------------------------------------------------

# 360 as total medals
sum(JO)

res.ca$call$marge.row

res.ca$call$marge.row * sum(JO)

res.ca$call$marge.col

res.ca$call$marge.col * sum(JO)




# ------------------------------------------------------------------------------
# Discoveries from the data
# ------------------------------------------------------------------------------

# Distinct separation betwen endurance events and sprinting event:
# 400m and 800m are separated in 1st dimension.
# The limit situated somewhere between the 400m and 800m
# 800m is similar to the endurance evens whereas 400m is similar to the sprinting events.


# Marathon is indeed an endurance event but is much more extreme than may have been expected.
# Similarly, the walking events (20km and 50km) are not endurance events in the same way as running races.


# The athlets who participate in sprinting events often tend to "double up" and run 100m and 200m or 200m and 400m


# ----------:  IMPORTANT for "quality" interpretation  :----------
# The two hurdle evens (110m and 400m hurdles) are somewhat different:
# the 110m hurdles is relatively far from the 100m whereas the 400m hurdles is close to the 400m.
# The 100m and 100m hurdles events draw on very different qualities:
# the 110m hurdles is a technical event whereas the 100m requires bursts of evergy,
# which explaines why no athletes participate in both of these events.
# However, the 400m hurdles is a much less techinical event than the 110m hurdles.
# It draws on qualities similar to those used in the 400m, which explains why athlets can participate in both events



# In the throwing events, the hammer and the discus are very similar whereas the shot put and the javelin are somewhat different.
# The hammer and the discus are thrown in rotation (with a leverage effect) while the javelin is thrown in a straight line.
# The shot put is thrown with or without rotation (and without leverage, as the shot must be in contact with the neck when thrown)



# The decatholon, the ultiamte all-round event, is opposed to the endurance events on the first dimension.
# This event is therefore not suitable for endurance athletes, generally speaking.
# Decathlon athletes' morphology, with a great deal of muscle mass and a capaticy for bursts of energy, does not help them
# in endurance events.
# Decathlon athletes often have difficulty finishing their event by a 1500m.





