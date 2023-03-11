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
# Fit a single linear model and conduct 10-fold CV to estimate the error
# ------------------------------------------------------------------------------
set.seed(1)
lm1Fit <- train(FE ~ EngDispl, 
                data = cars2010,
                method = "lm", 
                trControl = trainControl(method= "cv"))

lm1Fit



