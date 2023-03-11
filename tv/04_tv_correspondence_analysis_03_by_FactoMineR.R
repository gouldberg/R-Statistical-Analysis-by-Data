setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------

data("TV", package = "vcdExtra")


TV


dim(TV)


str(TV)



# ----------
TV2 <- margin.table(TV, c(1,3))

TV2



# ------------------------------------------------------------------------------
# Simple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

library(FactoMineR)


res.ca <- CA(TV2)



# ----------
# main results: inertia associated with each component, coordinates, contributions, 
# and representation qualities of the rows and columns

summary(res.ca)


# comparison with ca()



# ------------------------------------------------------------------------------
# Plot rows and columns separately
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

plot(res.ca, invisible = "col")

plot(res.ca, invisible = "row")




# ------------------------------------------------------------------------------
# Representing exact barycenters
# ------------------------------------------------------------------------------

# Coordinate * sqrt(eigenvalue)

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


summary(res.ca)


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
