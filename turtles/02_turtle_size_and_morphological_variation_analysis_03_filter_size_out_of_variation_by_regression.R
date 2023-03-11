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
# Filter size out of the variation with regression
#   - Consider size as a linear function of the measurements and consider shape as the remaining variation once variation explained by size has been filtered.
#   - Shape will correspond to the residual variance.
# ------------------------------------------------------------------------------

regmod <- lm(as.matrix(turtles[1:24,2:4]) ~ geosize[1:24])

regmod1 <- lm(as.matrix(turtles[1:24,2:4]) ~ geosize[1:24] - 1)



# -->
# Infiltering size by regression, one obtains a new combination of variables on which one can appraise the variation explained by the scalar size.
# Actually, this variation is isometric and also allometric variation because we authorized the intercept to be estimated by the model.

# A solution is to force the model to pass through the intercept.



# ----------
sum(diag(var(regmod$fitted.values))) / sum(diag(var(as.matrix(turtles[1:24,2:4]))))

sum(diag(var(regmod1$fitted.values))) / sum(diag(var(as.matrix(turtles[1:24,2:4]))))


# -->
# More than 96% of the morphological variation is explained by size alone in the first model,
# while only 84.5% is explained in the second.



# ----------
pairs(regmod$residuals, labels = c("res.length", "res.width", "res.height"), pch = 21, bg = "black", cex = 1.5)

# pairs(regmod1$residuals, labels = c("res.length", "res.width", "res.height"), pch = 21, bg = "black", cex = 1.5)



# -->
# We can observe that residual variaion in height and width covary in opposite directions.
# Although this relationship occurs not only because of overall size variation, it means that there is a trend in turtle carapace variation
# (flatter and higher shell) that may be related to some allometric relationship:  in becoming wider, turtles become flatter.



# ------------------------------------------------------------------------------
# Examine eigenvalues of the variance-covariance matrix of residulas
#   - what happens to the dimensionality of the new dataset ?
# ------------------------------------------------------------------------------

# Decompose a variance-vovariance matrix and yields singular values
svd(var(regmod$residuals))$d

svd(var(regmod1$residuals))$d


# -->
# The last singlar value is actually equal to zero.
# This is not the case in the second model. This is because of the degeneracy of regression.

