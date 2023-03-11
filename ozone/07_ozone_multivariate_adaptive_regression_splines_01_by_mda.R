setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)



# ------------------------------------------------------------------------------
# Multivariate Adaptive Regression Splines (MARS)
#  - The model building process proceeds iteratively. We start with no basis functions. We then search over all variables and possible knotpoints
#    t to fint the one basis function that produces the best fit to the data. We now repeat this process to find the next best basis function 
#    addition given that the first basis funciton is already included in the model.
#    The number of basis functions added determines the overall smoothness of the fit.
#  - When interactions are disallowed, the MARS approach will be a type of additive model.
#    The MARS approach will have a relative advantage when interactions are considered, particularly when there are a larger number of variables.
#  - The univariate version approach used a moderate number of knots in just on dimension, while MARS use a few knots in higher dimension.
#  - MARS can be favorably compared to linear regression: it has additional flexibility to find nonlinearity in the predictors in higher dimensions.
#  - MARS can also be favorably compared to the tree method: it allows for continuous fits but still rmaintains good interpretability.
# ------------------------------------------------------------------------------

# The default choice allows only additive (first-order) predictors and chooses the model size using a GCV criterion.
library(mda)

a <- mars(ozone[,-1], ozone[,1])

str(a)



# ----------
# The basis functions can be used as predictors in a linear regression model
olm <- lm(ozone[,1] ~ a$x - 1)

summary(olm)


# -->
# Fit is very good in terms of R2 (= 0.93), but the model size is also larger.



# ---------
# Reduce the model size to that used for previous models
# The parameter nk controls the maximum number of model terms
a2 <- mars(ozone[,-1], ozone[,1], nk = 7)

olm2 <- lm(ozone[,1] ~ a2$x - 1)

summary(olm2)



# ---------
# We allow second-order (two-way) interaction terms
a3 <- mars(ozone[,-1], ozone[,1], nk = 10, degree = 2)

olm3 <- lm(ozone[,1] ~ a3$x - 1)

summary(olm3)



# -->
# Good fit
# Since there are 9 predictors, this would mean 36 possible 2-way interaction terms.
# Such a model would be complex to estimate and interpret.
# In contrast, the MARS approach reduces the complexity by carefully selecting the interaction terms.

# MARS used a few knots in higher dimensions.


# ----------
# check how the terms enter into the model
# we can examine the actual form of the basis functions

a3$factor  # --> variable of vh, windo, dpg, ibt, vis are not selected

a3$factor[a3$selected.terms,]  # --> the first term, given by the first row, is the intercept and involves no variables


summary(olm3)
a3$factor[a3$selected.terms,]


# -->
# The "1" indicates a right hockey stick and the "-1" a left hockey stick.


# ----------
a3$factor  # --> ibh(4), and doy(6, 7)

# dipicting the effect of ibh(4) and doy(6, 7)
par(mfrow=c(1,2))
plot(ozone[,"ibh"], a3$x[,4] * a3$coef[4], xlab = "ibh", ylab = "Contribution of ibh")
plot(ozone[,"doy"], a3$x[,7] * a3$coef[7] + a3$x[,6] * a3$coef[6], xlab = "Day", ylab = "Contribution of day")



# ----------
summary(ozone)

# temperature and humidity have an interaction so we must combine all terms involving these
humidity <- seq(10, 100, len = 20)
temp <- seq(20, 100, len = 20)
medians <- apply(ozone, 2, median)

pdf <- matrix(medians, nrow = 400, ncol = 10, byrow = T)
pdf[,4] <- rep(humidity, 20)  # humidity is 4th column
pdf[,5] <- rep(temp, rep(20, 20))  # temp is 5th column
pdf <- as.data.frame(pdf)
names(pdf) <- names(medians)

z <- predict(a3, pdf[,-1])
zm <- matrix(z, ncol = 20, nrow = 20)

contour(humidity, temp, zm, xlab = "Humidity", ylab = "Temperature")
persp(humidity, temp, zm, xlab = "Humitidy", ylab = "Temperature", zlab = "Ozone", theta = -30)



# ----------
qqnorm(a3$res, main = "")
plot(a3$fit, a3$res, xlab = "Fitted", ylab = "Residuals")


# -->
# show no problme with normality, but some indication of nonconstant variance.

