setwd("//media//kswada//MyFiles//R//turtles")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  turtles
#   - data set of Jolicoeur and Mosimann
# ------------------------------------------------------------------------------

data("turtles", package = "Flury")


str(turtles)


car::some(turtles)



# ------------------------------------------------------------------------------
# Principal Component Analysis
#   - By using the principal component approach to decompose morphological variation, the size axis becoms a function of the original variables
#   - prcomp:  work on variance-covariance matrices --> singular-value decomposition
#   - princomp:  work on correlation matrices  --> spectral decomposition
# ------------------------------------------------------------------------------

# we perform the analysis for data that are all expressed in the same unit, theres is no reason to scale the data
# standard deviations are equal to singular values
( pr <- prcomp(turtles[1:24, 2:4]) )

( pr_log <- prcomp(log(turtles[1:24, 2:4])) )



# Rotation value contains the contribution of the original variables on principal components
summary(pr)
summary(pr_log)



# -->
# The first PC is positively related to all measurement
# It corresponds to the major axis of variation and can be interpreted as a size axis.
# The second PC contrasts width, and both length and heights, and thus corresponds to the relative width of the turtle carapace
# The last corresponds to the relative height and opposes high and low carapaces for individuals of similar size.

# The first axis represents 97.6% of the total variation, which is slightly more than when we used the regression model for filtering size
# (even when ignoring the intercept)
# This is normal, since like the major axis method, the PCA finds the fit that minimizes net distnace to the axes.
# In contrast, further axes correspond to shape axes.



# ------------------------------------------------------------------------------
# biplot
# ------------------------------------------------------------------------------

biplot(pr, xlim = c(-0.4, 0.5), col = 1)

biplot(pr_log, xlim = c(-0.4, 0.5), col = 1)



# -->
# The first component of a PCA of logged variables corresponds to a growth-axis,
# recording both isometric and allometric variations.

# The two remaining components correspond to variation not explained by isometry or allometry.
# In other words, they correspond to variation not explained by general growth.



# ------------------------------------------------------------------------------
# Understanding shape differences between groups:
#   - Examine the distribution of observations on the second and third axes of shape
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ELLI(): 
#   - Arguments:
#        - x: First variable (a numeric vector)
#        - y: Second variable (a numeric vector)
#        - conf: Confidence level in %
#        - np: Number of sampled points on the ellipse
#   - Values:
#        - Coordinates of points sampled on the ellipse
# ------------------------------------------------------------------------------

ELLI <- function(x, y, conf = 0.95, np = 50){
  
  centroid <- apply(cbind(x, y), 2, mean)
  
  ang <- seq(0, 2 * pi, length = np)
  
  z <- cbind(cos(ang), sin(ang))
  
  radiuscoef <- qnorm((1 - conf)/2, lower.tail = F)
  
  cvcxy <- var(cbind(x, y))
  
  r <- cor(x, y)
  
  M1 <- matrix(c(1, 1, -1, 1), 2, 2)
  
  M2 <- matrix(c(var(x), var(y)), 2, 2)
  
  M3 <- matrix(c(1 + r, 1 - r), 2, 2, byrow = T)
  
  ellpar <- M1 * sqrt(M2 * M3 / 2)
  
  t(centroid + radiuscoef * ellpar %*% t(z))
  
}

pca <- prcomp(turtles[1:24, 2:4])

( proj <- as.matrix(turtles[,2:4]) %*% pca[[2]] )



# takes 2nd and 3rd axis
plot(proj[,2:3], xlab = "PC2", ylab = "PC3", pch = 21, bg = c("black", "white")[turtles[,1]], asp = 1)

lines(ELLI(proj[1:24, 2], proj[1:24, 3]))
lines(ELLI(proj[25:48, 2], proj[25:48, 3]))


# -->
# males and femals differentiate on the third axis, which is related to relative height variation

# We can interpret the PCA in terms of shape difference between sexes.
# However, PCA is not the preferred way to explore shape differences.

