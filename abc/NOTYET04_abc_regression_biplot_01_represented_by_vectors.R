setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ----------
# convert factor to numeric
ABC2 <- rapply(ABC[,c(1,4,6:11)], f = as.numeric, classes = "factor", how = "replace")

str(ABC2)



# ------------------------------------------------------------------------------
# Regression Biplots of vector version
# (in the tradition of Gabriel (1971) and Greenacre (2010))
#   - We are interested in mapping two IQ variables (VIQ as y1 and PIQ as y2) and body weight (y3) into the scatterplot
#     defined by body height (x1) and brain size (x2).
# ------------------------------------------------------------------------------

# Relabel persons
rownames(ABC2) <- 1:nrow(ABC2)


# Standardize
ABC2 <- as.data.frame(scale(ABC2))

car::some(ABC2)



# ----------
# regressions which give us the set of standardized beta-coefficients
regfit <- lm(cbind(satis, recom) ~ -1 + equipment + sales + technical + training + purchase + pricing, data = ABC2)

summary(regfit)

regfit



# ----------
# vector coordinates
colnames(regfit$coef, 3)


# The combination of coefficients to Height and MRI_Count will be vectors represented in variable space.
round(regfit$coef, 3)



# ----------
summary(regfit)[["Response satis"]]$r.squared

R2vec <- sapply(summary(regfit), '[[', "r.squared")

R2vec



# ----------
# Regression Biplots
# For all biplots it is important to set the aspect ratio to 1 by means of asp = 1, such that distances between points are represented accurately on screen.
par(mfrow = c(1, 1))

plot(ABC2$satis, ABC2$recom, pch = 20, col = "gray", xlab = "satis (standardized)",
     ylab = "recom (standardized)", main = "Regression Biplot", asp = 1)

abline(h = 0, v = 0, lty = 2, col = "darkgray")

arrows(0, 0, regfit$coef[1,1], regfit$coef[2,1], length = 0.1, col = "brown")
text(regfit$coef[1,1], regfit$coef[2,1], labels = "satis", col = "brown", pos = 2, cex = 0.9)

arrows(0, 0, regfit$coef[1,2], regfit$coef[2,2], length = 0.1, col = "brown")
text(regfit$coef[1,2], regfit$coef[2,2], labels = "recom", col = "brown", pos = 3, cex = 0.9)



# ----------
# Vectors that point in a similar direction correspond to variables that have similar response profiles
# (they are correlated with each other)

round(cor(ABC2), 3)




# ----------
# The vector length y of each response corresponds to the sum of the squared standardized regression coefficients.
sqrt(sum(regfit$coef[,"satis"]^2))

sqrt(sum(regfit$coef[,"recom"]^2))
