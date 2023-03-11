setwd("//media//kswada//MyFiles//R//brainIQ")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brainIQ
# ------------------------------------------------------------------------------

data("BrainIQ", package = "MPsychoR")

str(BrainIQ)



# ----------
# omit NAs and gender
BrainIQ <- na.omit(BrainIQ[, -1])



# ------------------------------------------------------------------------------
# Regression Biplots of vector version
# (in the tradition of Gabriel (1971) and Greenacre (2010))
#   - We are interested in mapping two IQ variables (VIQ as y1 and PIQ as y2) and body weight (y3) into the scatterplot
#     defined by body height (x1) and brain size (x2).
# ------------------------------------------------------------------------------

# Relabel persons
rownames(BrainIQ) <- 1:nrow(BrainIQ)


# Standardize
BrainIQ1 <- as.data.frame(scale(BrainIQ))

car::some(BrainIQ1)



# ----------
# regressions which give us the set of standardized beta-coefficients
regfit <- lm(cbind(VIQ, PIQ, Weight) ~ -1 + Height + MRI_Count, data = BrainIQ1)

summary(regfit)

regfit



# ----------
# vector coordinates
colnames(regfit$coef, 3)


# The combination of coefficients to Height and MRI_Count will be vectors represented in variable space.
round(regfit$coef, 3)



# ----------
summary(regfit)[["Response VIQ"]]$r.squared

R2vec <- sapply(summary(regfit), '[[', "r.squared")

R2vec



# ----------
# Regression Biplots
# For all biplots it is important to set the aspect ratio to 1 by means of asp = 1, such that distances between points are represented accurately on screen.
par(mfrow = c(1, 1))
plot(BrainIQ1$Height, BrainIQ1$MRI_Count, pch = 20, col = "gray", xlab = "Body Height (standardized)",
     ylab = "Brain Size (standardized)", main = "Regression Biplot", asp = 1)
abline(h = 0, v = 0, lty = 2, col = "darkgray")
arrows(0, 0, regfit$coef[1,1], regfit$coef[2,1], length = 0.1, col = "brown")
text(regfit$coef[1,1], regfit$coef[2,1], labels = "VIQ", col = "brown", pos = 2, cex = 0.9)
arrows(0, 0, regfit$coef[1,2], regfit$coef[2,2], length = 0.1, col = "brown")
text(regfit$coef[1,2], regfit$coef[2,2], labels = "PIQ", col = "brown", pos = 3, cex = 0.9)
arrows(0, 0, regfit$coef[1,3], regfit$coef[2,3], length = 0.1, col = "brown")
text(regfit$coef[1,3], regfit$coef[2,3], labels = "Weight", col = "brown", pos = 4, cex = 0.9)



# -->
# Body height has a high impact on body weight,
# whereas brain size has a low impact on body weight.
# For both IQ variables, brain size has a positive impact, where as body height is weakly related to them (in negative direction).



# ----------
# Vectors that point in a similar direction correspond to variables that have similar response profiles
# (they are correlated with each other)

round(cor(BrainIQ[, 2:4]), 3)


# -->
# We see that VIQ and PIQ are highly correlated with each other; correspondingly their vectors point in a similar direction.
# Their correlation with weights is close to 0.
# Thus both IQ vectors are almost perpendicular to the weight vector.



# ----------
# The vector length y of each response corresponds to the sum of the squared standardized regression coefficients.
sqrt(sum(regfit$coef[,"VIQ"]^2))

