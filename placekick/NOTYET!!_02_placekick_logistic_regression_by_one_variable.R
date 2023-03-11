setwd("//media//kswada//MyFiles//R//placekick")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  placekick
# ------------------------------------------------------------------------------

placekick <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter2//Placekick.csv")

str(placekick)

car::some(placekick)



# ------------------------------------------------------------------------------
# Logistic Regression with "distance" variable
# ------------------------------------------------------------------------------

mod.fit <- glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)

summary(mod.fit)



# ------------------------------------------------------------------------------
# Logistic Regression with "distance" variable
# ------------------------------------------------------------------------------
# Show Coefficients table only
temp<-summary(mod.fit)
names(temp)
temp$coefficients
round(summary(mod.fit)$coefficients,4)  # Easier way to get the same result, I used round due to the width of the book


# Estimated covariance matrix
vcov(mod.fit)
vcov(mod.fit)[2,2]  # Var^(beta^_1)
summary(mod.fit)$coefficients[2,2]^2  # Var^(beta^_1)


# Matrix calculations
pi.hat<-mod.fit$fitted.values
V<-diag(pi.hat*(1-pi.hat))
V[1:3,1:3]
X<-cbind(1, placekick$distance)  # Could also use model.matrix()
solve(t(X) %*% V %*% X)


# Part of the IRLS - this would be the next step if convergence had not already been obtained.
Y<-placekick$good
Z<-log(pi.hat/(1-pi.hat)) + diag(1/(pi.hat*(1-pi.hat)))%*%(Y-pi.hat) 
solve(t(X)%*%V%*%X)%*%t(X)%*%V%*%Z   # One form
mod.fit$coefficients + solve(t(X)%*%V%*%X)%*%t(X)%*%(Y-pi.hat)  # Another form

