setwd("//media//kswada//MyFiles//R//work_women")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  work_women
# ------------------------------------------------------------------------------

work <- read.table("work_women.csv", header = TRUE, row.names = 1, sep = ";")

str(work)

dim(work)


colnames(work) <- c("stay.at.home", "part.time.work", "full.time.work", "totally.agree", "mostly.agree", "mostly.disagree", "totally.disagree")

names(work)

work



# ----------
# we divide into two tables
( work1 <- work[,1:3] )

( work2 <- work[,4:7] )



# ------------------------------------------------------------------------------
# Simple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

res.ca <- CA(work1)


# main results: inertia associated with each component, coordinates, contributions, 
# and representation qualities of the rows and columns
summary(res.ca)


# comparison with ca()
# summary(work.ca)



# ------------------------------------------------------------------------------
# Plot rows and columns separately
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

plot(res.ca, invisible = "col")

plot(res.ca, invisible = "row")



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



# -->
# In latter plot, the "both parents work equally (in row)" is associated more or less equally with the categories
# "part-time" and "full-time" and, in an exact barycenbtric representation.
# The aim of an exact barycentric representation is to be able to visualise the intensity of the relationship expressed by the plane.
# Simultaneous representation produced by the CA is designed to visualise the nature of the relationship between the variables and tell us nothing
# about its intensity.





# ------------------------------------------------------------------------------
# Coordinates, relative contributions (in %), and Representation, Quality for Each Category and Dimension
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
plot(res.ca)


summary(res.ca)


# ----------
# contribution:
# the contribution of point i to the inertia of the dimension of rank s
# the atypical case of a dimension caused by only one or two points can thus be detected immediately

# --> The point "both parents work equally" is roughly twice as far from the origin as the other, thus suggesting a greater influence.
# However, the weight of "both parents work equally" is roughly three time less, therfore simultaneously suggesting a lesser influence.
# As far as inertia is concerned, it is the square of the distance which intervenes: thus, in the end, both contributions balance out.


# ----------
# quality of points on a dimension:
# indicates how the deviation of category i from the average profile is expressed on the dimension of rank s.

# -->
# "part-time work" is ineffectively represented by the 1st dimension, but this doesno t necessarily mean that we should disregard it. 



# ----------
# Distance (squared) to Average Profile and Inertia (in Initial Spaces)
res.ca$row$inertia / res.ca$call$marge.row
res.ca$row$inertia

res.ca$col$inertia / res.ca$call$marge.col
res.ca$col$inertia


# -->
# Distances does not take the category's sample size into account.
