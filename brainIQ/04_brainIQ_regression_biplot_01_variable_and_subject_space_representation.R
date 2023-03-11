setwd("//media//kswada//MyFiles//R//brainIQ")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brainIQ
# ------------------------------------------------------------------------------

data("BrainIQ", package = "MPsychoR")

str(BrainIQ)




# ------------------------------------------------------------------------------
# Regression Biplots
#   - We are interested in mapping two IQ variables (VIQ as y1 and PIQ as y2) and body weight (y3) into the scatterplot
#     defined by body height (x1) and brain size (x2).
# ------------------------------------------------------------------------------

# centered VIQ and PIQ
x <- as.vector(scale(BrainIQ$VIQ, scale = FALSE))

y <- as.vector(scale(BrainIQ$PIQ, scale = FALSE))



# ----------
# Compute Euclidean norm
Enorm <- function(x) sqrt(sum(x^2))


# VIQ vector
xn <- Enorm(x)



# ----------
# PIQ vector
yn <- Enorm(y)


# angle (in radians):  correlation r(x, y) = cos(theta)
( theta <- acos(crossprod(x, y) / (Enorm(x) * Enorm(y))) )


# x-coordinate y vector (orthogonal y projection on x)
y1 <- yn * cos(theta)


# y-coordinate y vector (orthogonal y projection on vector orthogonal to x)
y2 <- yn * sin(theta)




# ----------
graphics.off()
op <- par(mfrow = c(1,2))


# variable space representation:  scatterplot of centered VIQ and PIQ
plot(BrainIQ$VIQ, BrainIQ$PIQ, pch = 19, xlab = "VIQ", ylab = "PIQ", main = "IQ Variable Space")


# subject space representation: VIQ vector and PIQ vector
# The length of the vectors reflect the dispersion of each variable (precisely: sigma(x) * sqrt(n - 1) with n as the number of observations)
# The angle theta between the vectors translates into a correlation r(x, y) = cos(theta)
plot(0:160, 0:160, ylim = c(0, 110), type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,  main = "IQ subject space")
arrows(0, 0, xn, 0, length = 0.10, lwd = 2)
text(xn, 0, "VIQ", pos = 4, offset = 0.4)
arrows(0, 0, y1, y2, length = 0.10, lwd = 2)
text(y1, y2, "PIQ", pos = 4, offset = 0.4)
par(op)

