setwd("//media//kswada//MyFiles//R//hvs")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HVS
# ------------------------------------------------------------------------------
HVS <- read.table(file = "HVS.txt", header = TRUE, dec=".")


str(HVS)

names(HVS)


# ----------
# The variable names are long, we will rename some of them
# We also define categorical variables as factors, to avoid mistakes.
HVS$OrbitalV    <- HVS$MeanOrbitalVolume
HVS$fPopulation <- factor(HVS$Population)
HVS$LatAbs      <- HVS$AbsoluteLatitude
HVS$CC          <- HVS$CranialCapacity
HVS$FM          <- HVS$FMarea_intercondyle
HVS$Illuminance <- HVS$Minimum_Illuminance
HVS$Temperature <- HVS$Minimum_Temperature_celsius
HVS$fGender     <- factor(HVS$Gender)



# ------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------

# There is one missing value
colSums(is.na(HVS))


# we remove it
# The function na.exvlude removes all rows that contain a missing value.
HVS2 <- na.exclude(HVS)



# ------------------------------------------------------------------------------
# linear regression
#   - In line with Pearce and Dunbar (2011) we decide to use absolute latitude and drop temperature and illuminance.
#     This means that if absolute latitude is significant in the model, it could represent a temperature or illuminance effect.
#     We also decide to drop gender, thus we use CC, LatAbs, and FM in the analysis.
# ------------------------------------------------------------------------------

M1 <- lm(OrbitalV ~ LatAbs, data = HVS)


print(summary(M1), digits = 2, signif.stats = FALSE)
# print(summary(M1), digits = 2, signif.stats = TRUE)



# ----------
# scatter plot with a fitted regression line and vertical dotted lines representing residuals
plot(x = HVS$LatAbs, y = HVS$OrbitalV, xlab = "LatAbs", ylab = "OrbitalV", cex.lab = 1.5)
abline(M1)

N <- nrow(HVS)
F1 <- fitted(M1)
for (i in 1:N) segments(HVS$LatAbs[i], HVS$OrbitalV[i], HVS$LatAbs[i], F1[i], lty = 2)



# ------------------------------------------------------------------------------
# linear regression:  calculate coefficients by matrix calculation
# ------------------------------------------------------------------------------
X <- model.matrix(M1)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% HVS$OrbitalV
betaHat



# ------------------------------------------------------------------------------
# linear regression:  calculate hat matrix
# ------------------------------------------------------------------------------
( H <- X %*% solve(t(X) %*% X) %*% t(X) )


# diagonal element of H
hat(X)
diag(H)



# ----------
# the fitted values are equal to a matrix H multiplied by the original data.
# So the fitted value for observation i is a weighted average of the original data.
predict(M1)
( y_hat <- H %*% HVS$OrbitalV )




# ------------------------------------------------------------------------------
# linear regression:  residuals
# ------------------------------------------------------------------------------
# the residulas e can be calculated as
( e  <- resid(M1) )
HVS$OrbitalV - y_hat



# standardized residuals
# It is advisable to work with the standardized residuals, as these provide a better estimator of the real residuals
estd <- rstandard(M1)




# ------------------------------------------------------------------------------
# leverage values for each observation
# ------------------------------------------------------------------------------
# leverage values are the diagnal elements of H, quantifying how extreme an observation is in terms of the explanatory variables,
# indicating whether an observation is potentially influential.
# It is represented by a number between 0 and 1; the higher the leverage, the more extreme the covariate values of an observation.

plot(diag(H), type = "h", xlab = "Crania", ylab = "Leverage", cex.lab = 1.5)




# ------------------------------------------------------------------------------
# Cook distance
#   - To confirm whether an observation is influential, we can compare the fitted values obtained using all 55 observations with those obtained when
#     dropping the ith observation.
#   - A large sum of squared differences between the two sets of fitted values would indicate that the ith observation is influential.
#
#   - Di = | y_hat - y_hat(i) |^2 / p * sigma_hat^2
#     p: the number of regression parameters in the model
#     (i) referes to the fitted values in which observation i was omitted
# ------------------------------------------------------------------------------
plot(cooks.distance(M1), type = "h")


plot (M1, which = 4)



# ------------------------------------------------------------------------------
# Standard errors and confidence intervals for parameter
# ------------------------------------------------------------------------------
SE <- summary(M1)$sigma * sqrt(diag(solve(t(X) %*% X)))

SE



# Obtain confidence intervals
qt(1-0.05/2,df =55-2)

Z <- rbind(coef(M1) - qt(1-0.05/2,df =55-2) * SE, coef(M1) + qt(1-0.05/2,df =55-2) * SE)
rownames(Z) <- c("Lower bound","Upper bound")
Z



# ------------------------------------------------------------------------------
# confidence intervals and prediction intervales for fitted values
# ------------------------------------------------------------------------------
# Obtain the covariance matrix
covBeta <- vcov(M1)



# Create the data frame
MyData <- data.frame(LatAbs = seq(from = 0.02,  to = 65, length = 10))
X <- model.matrix(~LatAbs, data = MyData)



# Calculate the SEs
MyData$P <- X %*% coef(M1)
MyData$SEy.ci <- sqrt(diag(X %*% covBeta %*% t(X)))
MyData$SEy.pi <- sqrt(diag(X %*% covBeta %*% t(X) + summary(M1)$sigma^2))



# using appropriate values
t1 <- qt(1-0.05/2,df =55-2)



# lower and upper limits for 95%
MyData$ci.ll <- MyData$P - t1 * MyData$SEy.ci
MyData$ci.ul <- MyData$P + t1 * MyData$SEy.ci



# ----------
# for prediction intervals
MyData$pi.ll <- MyData$P - t1 * MyData$SEy.pi
MyData$pi.ul <- MyData$P + t1 * MyData$SEy.pi

plot(x = HVS2$LatAbs, y = HVS2$OrbitalV, pch = 16, cex = 0.8, xlab = "Absolute latitude", ylab = "Orbital volume")

with(MyData,{
  polygon(c(LatAbs, rev(LatAbs)), c(ci.ll, rev(ci.ul)), col = grey(0.4), border = NULL, density =70)
  polygon(c(LatAbs, rev(LatAbs)), c(pi.ll, rev(pi.ul)), col = grey(0.5), border = NULL, density =20)
  lines(LatAbs, P, lwd = 3)
})



# ----------
# We can also use the predict function
( PI <- predict(M1, newdata = MyData, se.fit = TRUE, interval = "predict") )

( CI <- predict(M1, newdata = MyData, se.fit = TRUE, interval = "confidence") )



