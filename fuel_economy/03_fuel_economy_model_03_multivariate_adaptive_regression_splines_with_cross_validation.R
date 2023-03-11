setwd("//media//kswada//MyFiles//R//fuel_economy")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "earth")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fuel Economy
# ------------------------------------------------------------------------------
data("FuelEconomy", package = "AppliedPredictiveModeling")

dim(cars2010)
dim(cars2011)
dim(cars2012)


car::some(cars2010)


# -->
# Models can be created using the 2010 data (containing 1,107 vehicles) and tested on the 245 new 2011 cars.



# ------------------------------------------------------------------------------
# Finally a MARS model (via the earth package)
#  - When used with a single predictor, MARS (multivariate adaptive regression spline) model can fit separate linear regression lines
#    for different ranges of engine displacement.  The slopes and intercepts are estimated for this model, as well as the number and size
#    of the separate regions for the linear models.
# ------------------------------------------------------------------------------
set.seed(1)

marsFit <- train(FE ~ EngDispl, 
                 data = cars2010,
                 method = "earth",
                 tuneLength = 15,
                 trControl = trainControl(method= "cv"))

marsFit

plot(marsFit)



# -->
# For a single predictor, MARS can allow for up to five model terms. Using cross-validation, we evaluated 4 condidate values for
# this tuning parameter to create the resampling profile.
# The lowest RMSE value is associated with 4 terms, although the scale of change in the RMSE value is indicates that 
# there is some insensitivity to this tuning parameter.



# ------------------------------------------------------------------------------
# Predict the test set data
# ------------------------------------------------------------------------------
cars2011$lm1  <- predict(lm1Fit,  cars2011)
cars2011$lm2  <- predict(lm2Fit,  cars2011)
cars2011$mars <- predict(marsFit, cars2011)


postResample(pred = cars2011$lm1,  obs = cars2011$FE)
postResample(pred = cars2011$lm2,  obs = cars2011$FE)
postResample(pred = cars2011$mars, obs = cars2011$FE)



