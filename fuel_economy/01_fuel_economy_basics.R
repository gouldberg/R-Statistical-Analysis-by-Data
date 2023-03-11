setwd("//media//kswada//MyFiles//R//fuel_economy")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fuel Economy
#   - The fueleconomy.gov web site, run by the U.S. Department of Energy's Office of Energy Efficiency and Renewable Energy and the U.S. Environmental
#     Protection Agency, lists different estimates of fuel ecomony for passenger cars and trucks.
#     For each vehicle, various characteristics are recorded such as the engine displacement or number of cylinders.
#     Along with these values, laboratory measurements are made for the city and highway miles per gallon (MPG) of the car.
# ------------------------------------------------------------------------------
data("FuelEconomy", package = "AppliedPredictiveModeling")

dim(cars2010)
dim(cars2011)
dim(cars2012)



# ------------------------------------------------------------------------------
# Preparation
# ------------------------------------------------------------------------------
# Sort by engine displacement
cars2010 <- cars2010[order(cars2010$EngDispl),]
cars2011 <- cars2011[order(cars2011$EngDispl),]



# ----------
# Combine data into one data frame
cars2010a <- cars2010
cars2010a$Year <- "2010 Model Year"
cars2011a <- cars2011
cars2011a$Year <- "2011 Model Year"


plotData <- rbind(cars2010a, cars2011a)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
