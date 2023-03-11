setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# Multivariate Adaptive Regression Splines (MARS)
#  - The MARS approach will have a relative advantage when interactions are considered, particularly when there are a larger number of variables.
#  - The univariate version approach used a moderate number of knots in just on dimension, while MARS use a few knots in higher dimension.
#  - MARS can be favorably compared to linear regression: it has additional flexibility to find nonlinearity in the predictors in higher dimensions.
#  - MARS can also be favorably compared to the tree method: it allows for continuous fits but still rmaintains good interpretability.
# ------------------------------------------------------------------------------

library(mda)


# ----------
# The default choice allows only additive (first-order) predictors and chooses the model size using a GCV criterion.
a <- mars(ethanol[,c("C", "E")], ethanol[,"NOx"])

a



# ----------
# The basis functions can be used as predictors in a linear regression model
olm <- lm(ethanol[,"NOx"] ~ a$x - 1)

summary(olm)


# -->
# Fit is very good in terms of R2 (= 0.99)
# but some term is not "***" but "**"



# ---------
# We allow second-order (two-way) interaction terms
a3 <- mars(ethanol[,c("C", "E")], ethanol[,"NOx"], nk = 10, degree = 2)

olm3 <- lm(ethanol[,"NOx"] ~ a3$x - 1)

summary(olm3)


# -->
# Good fit and all the terms are "***" at significant level
# In contrast, the MARS approach reduces the complexity by carefully selecting the interaction terms.



# ----------
# check how the terms enter into the model
# we can examine the actual form of the basis functions

a3$factor

a3$selected.terms

a3$factor[a3$selected.terms,]  # --> the first term, given by the first row, is the intercept and involves no variables


summary(olm3)
a3$factor[a3$selected.terms,]


# -->
# The "1" indicates a right hockey stick and the "-1" a left hockey stick.


# ----------
a3$factor[a3$selected.terms,]  # --> we will dipict C effect by 4,6 terms, and dipict E effect by 2,3,4,5 terms

par(mfrow=c(1,2))
plot(ethanol[,"C"], a3$x[,4] * a3$coef[4] + a3$x[,6] * a3$coef[6], 
     xlab = "C", ylab = "Contribution of C")
plot(ethanol[,"E"], a3$x[,2] * a3$coef[2] + a3$x[,3] * a3$coef[3] + a3$x[,4] * a3$coef[4] + a3$x[,5] * a3$coef[5], 
     xlab = "E", ylab = "Contribution of E")



# ----------
summary(ethanol)

# C and E have an interaction so we must combine all terms involving these
C <- seq(5, 20, len = 20)
E <- seq(0.5, 1.5, len = 20)
( medians <- apply(ethanol, 2, median) )

pdf <- matrix(medians, nrow = 400, ncol = 3, byrow = T)
pdf[,2] <- rep(C, 20)  # C is 2th column
pdf[,3] <- rep(E, rep(20, 20))  # E is 3th column
pdf <- as.data.frame(pdf)
names(pdf) <- names(medians)

NOx <- predict(a3, pdf[,-1])
NOxm <- matrix(NOx, ncol = 20, nrow = 20)

contour(C, E, NOxm, xlab = "C", ylab = "E")
persp(C, E, NOxm, xlab = "C", ylab = "E", zlab = "NOx", theta = -30)


# ----------
qqnorm(a3$res, main = "")
plot(a3$fit, a3$res, xlab = "Fitted", ylab = "Residuals")


# -->
# show no problme with normality, but some indication of nonconstant variance.


