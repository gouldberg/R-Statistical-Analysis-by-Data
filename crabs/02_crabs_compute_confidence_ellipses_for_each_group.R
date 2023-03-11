setwd("//media//kswada//MyFiles//R//crabs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crabs
# ------------------------------------------------------------------------------

data("crabs", package = "MASS")


str(crabs)


car::some(crabs)



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

# i <- 1
# a <- levels(crabs$sp:crabs$sex)[i]
# x <- crabs$FL[crabs$sp:crabs$sex == a]
# y <- crabs$RW[crabs$sp:crabs$sex == a]
# conf <- 0.95
# np <- 50


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



# ------------------------------------------------------------------------------
# Analyizing the relationship between two distance measurements in different groups
# Compute confidence ellipses
# ------------------------------------------------------------------------------

coul <- rep(c("grey40", "black"), 2)

lwe <- c(2, 2, 1, 1)


plot(crabs$FL, crabs$RW, bg = c("grey50", "white")[crabs$sex], pch = c(21, 22)[crabs$sp], cex = c(1.8, 1)[crabs$sp])

for(i in 1:4){
  
  a <- levels(crabs$sp:crabs$sex)[i]
  lines(ELLI(
    crabs$FL[crabs$sp:crabs$sex == a],
    crabs$RW[crabs$sp:crabs$sex == a]),
    col = coul[i], lwd = lwe[i])
}



# -->
# 95% confidence ellipses for the crab measurements of the genus Lpetograpsus according to species and sex

# The species factor seems to be related to the overall size of the crabs since both measurements seem slightly smaller in one species.

# Measurements are strongly related, and this is likely to be because of size variation; 
# we have small and large individuals for each sample

# Small individuals are more similar than large individuals.
# Differentiation according to sex and species seems to occur because of growth

# Some differences seem to be more related to the relationships between variables rather than in the dipersion of the observations.
# Relationships between variables are the exression of shape difference since they correspond to differences in proportion
# or in proportional change during growth.

# If one estimates regression parameters using a linear model, one usually finds that the intercept is significantly different from zero
# this is because the relationships between measurement variables are not constant or not the simple expression of a linear expression
# during growth.

