setwd("//media//kswada//MyFiles//R//eu15")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EU15
#   - The data were analyzed by Voudouris et al.
#   - Variables:
#       - GDP:  Sum of GDP of the EU 15 countries
#       - Year:  Year (19060 to 2009)
#       - UsefuleEnergy:  Total amount of usefule energy (energy that performs some for of 'work') for the EU 15 countries)
#       - Labor:  Total hours worked in the EU15 countries
#       - Capital:  Sum of net capital stock of the EU15 countries
# ------------------------------------------------------------------------------
data("eu15", package = "gamlss.data")


str(eu15)

car::some(eu15)



# ----------
eu15 <- transform(eu15, lGDP = log(GDP), lCapital = log(Capital), lLabor = log(Labor), lUsefulEnergy = log(UsefulEnergy))

car::some(eu15)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

car::scatterplot(lGDP ~ Year, data = eu15)



# ----------
car::scatterplot(lGDP ~ lCapital, data = eu15)

car::scatterplot(lGDP ~ lUsefulEnergy, data = eu15)

car::scatterplot(lGDP ~ lLabor, data = eu15)



# ------------------------------------------------------------------------------
# basics:  Check the year variation
# ------------------------------------------------------------------------------
rbPal <- colorRampPalette(c("red", "blue"))

col <- rbPal(5)[as.numeric(cut(eu15$Year, breaks = 5))]


with(eu15, plot(lLabor, lGDP, pch = 21, col = col, main = "Labor"))

with(eu15, plot(lCapital, lGDP, pch = 21, col = col, main = "Capital"))

with(eu15, plot(lUsefulEnergy, lGDP, pch = 21, col = col, main = "UsefulEnergy"))



# -->
# Labor plot shows clearly that high values are associated with different levels of GDP and time period
# (dark red indicates around 1960, dark blue around 2009), while there is a clearere relationship with Capital and UsefulEnergy (all on the log scale)


