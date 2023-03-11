setwd("//media//kswada//MyFiles//R//motorins")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  motorins
#   - In Hallin and Ingenbleek (1983) data on payments for insurance claims for various areas of Sweden in 1977 are presented.
#     The data is further subdivided by mileage driven, the bonum from not having made precious claims and the type of car.
# ------------------------------------------------------------------------------

data(motorins, package="faraway")

str(motorins)

car::some(motorins)



# ------------------------------------------------------------------------------
# check box-cox trans 
# ------------------------------------------------------------------------------
library(caret)

bc <- BoxCoxTrans(motorins$Payment)

bc


par(mfrow = c(2,1), mar = c(2,2,2,2))
hist(motorins$Payment)
hist(predict(bc, motorins$Payment), breaks=seq(3, 20, 1))


# -->
# Box-Cox trans suggests the use of a log transformation (lambda = 0) on the response



# ------------------------------------------------------------------------------
# Fit gamma GLM and select the model using the AIC criterion
#   - Gamma GLM is useful where we may be willing to speculate on the relationship between the mean and variance of the response
#     but are not sure about the distribution.
#   - For some data, we might expect the standard deviaion to increase linearly with the response:
#     SD( Y / E(Y) ) is constant but var(Y) is proportional to E(Y)^2
#     For example, measurements of larger objects do tend to have more error than smaller ones.
#     If we wanted to apply a Gaussian linear model, the log transform is indicated. This would imply a lognormal distribution for the original response.
#     Alternatively, if Y ~ gamma, then var Y is proportional to E(Y)^2, so a gamma GLM is also appropriate in this situation
# ------------------------------------------------------------------------------

# Since we expect that the total amount of the claims for a group will be proportionate to the number of insured,
# it makes sense to treat the log of the number insured as an offset

# attention has been restricted to data from Zone 1

motori <- motorins[motorins$Zone == 1,]

gl <- glm(Payment ~ offset(log(Insured)) + as.numeric(Kilometres) + Make + Bonus, family = Gamma(link=log), motori)

summary(gl)


# Deviance expalined = 1 - Residual Deviance / Null Deviance
1 - gl$deviance / gl$null.deviance




# ------------------------------------------------------------------------------
# For comparison, the lognormal model
# ------------------------------------------------------------------------------

llg <- glm(log(Payment) ~ offset(log(Insured)) + as.numeric(Kilometres) + Make + Bonus, family = gaussian, motori)

summary(gl)
summary(llg)



# -->
# Mileage class give by Kilometers is statistically significant in the gamma GLM, but not in the lognormal model.
# Some of the coefficients are quite different.
# For example, we see that for make 8, relative to the reference level of make 1, there are exp(0.7504) = 2.1178 times as much payment
# when using the gamma GLM,
# while the comparable figure for the lognormal model is exp(0.20958) = 1.2332



# ----------
AIC(gl);  AIC(llg);


# -->
# When computing a likelihood, it is common practive to discard parts that are not funcitons of the parameters.
# This has no consequence when models with same distribution for the response are compared since the parts discarded will be equal.
# For responses with different distributions, it is essential that all parts of the likelihood be retained.
# The large difference in AIC for these two models indicates that this precaustion was not taken.



# ----------
# Deviance expalined = 1 - Residual Deviance / Null Deviance
1 - gl$deviance / gl$null.deviance
1 - llg$deviance / llg$null.deviance


# -->
# Nevertheless, we note that the null deviance for both models is almost the same
# while the residual deviance is smaller for the gamma GLM.

# This improvement relative to the null indicates that the gamma GLM should be preferred here.



# ------------------------------------------------------------------------------
# Comparison of distributions:  gamma and lognormal
# ------------------------------------------------------------------------------
# compare the shapes of the distributions for the reponse using the dispersion estimates from the two models
# The means have been set to one in both cases.
summary(gl)
summary(llg)


( disp_gl <- summary(gl)$dispersion )
( disp_llg <- summary(llg)$dispersion)


x <- seq(0, 5, by = 0.05)
par(mfrow = c(1,2))
plot(x, dgamma(x, 1/disp, scale=disp), type = "l", ylab = "", xlab = "", yaxs = "i", ylim = c(0,1))
plot(x, dlnorm(x, meanlog = -disp_llg/2, sdlog = sqrt(disp_llg)), type = "l", ylab = "", xlab = "", yaxs = "i", ylim = c(0,1))


# -->
# We see the greater peakedness of the lognormal indicating more small payments which are balanced by more large payments.
# The lognormal thus has higher kurtosis.


# ----------
# compare predictions
x0 <- data.frame(Make = "1", Kilometres = 1, Bonus = 1, Insured = 100)


# gamma GLM
predict(gl, new=x0, se=T, type ="response")


# lognormal model
predict(llg, new=x0, se=T, type ="response")


# so the corresponding values on the original scale would be (by delta method to estimate the standard error on original scale)
c(exp(10.998), exp(10.998)*0.16145)




# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------

plot(gl, 1:2)


# -->
# We can see quite underdispersion ...

