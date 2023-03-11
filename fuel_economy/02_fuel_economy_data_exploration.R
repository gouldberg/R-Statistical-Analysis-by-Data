setwd("//media//kswada//MyFiles//R//fuel_economy")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fuel Economy
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
# Visualize the relationship between engine displacement and fuel efficiency each by year
#
#  - We here focus on unadjusted highway MPG for 2010-2011 model year cars
# ------------------------------------------------------------------------------
xyplot(FE ~ EngDispl | Year, plotData,
       xlab = "Engine Displacement",
       ylab = "Fuel Efficiency (MPG)",
       between = list(x = 1.2))


# -->
# Clearly, as engine displacement increases, the fuel efficiency drops regardless of year.
# The relationship is somewhat linear but does exhibit some curvature towards the extreme ends of the displacement axis.

