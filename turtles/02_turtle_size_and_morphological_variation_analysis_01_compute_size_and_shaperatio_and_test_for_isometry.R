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
# Compute the size and shape ratios
# ------------------------------------------------------------------------------

( geosize <- (apply(turtles[,2:4], 1, prod)) ^ (1 / length(turtles[1,2:4]) ) )


( shaperatio <- as.matrix(turtles[,2:4] / geosize) )



# ----------
pairs(log(shaperatio), pch = 21, cex = 1.5, bg = c("black", "white")[turtles[,1]])


# -->
# We see that males differ from females in being relatively higher, narrower and shorter.



# ------------------------------------------------------------------------------
# Test for isometry
# ------------------------------------------------------------------------------

# One can log-transform the shape ratios, and compare them with log-sizes to check for allometric or isometric variation.
# The analysis of variance of size variation explained by log-shape variables in a multivariate regression provides a test for isometry

mod <- lm(I(log(geosize[1:24])) ~ I(log(shaperatio[1:24,])))

summary(mod)


anova(mod)


# -->
# The relationship between variables is not purely isometric, as demonstrated by the significant F-value




