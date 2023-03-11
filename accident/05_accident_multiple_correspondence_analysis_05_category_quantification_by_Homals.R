setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ----------
# convert table to case form
acci_c <- expand.dft(acci.tab)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis Using Homals
#   - French Approach to multiple CA:  use a singular value decomposition on either the Burt matrix or the indicator matrix
#   - Dutch Approach (i.e., Homals) solves the multiple CA problem numerically, which offers a great amount of flexibility
# ------------------------------------------------------------------------------

library(Gifi)

acci.hom <- homals(acci_c)

summary(acci.hom)



# -->
# variance accounted for 1st 2 dimensions is 46.75%
# (slightly increased from 40.9% by mjca())



# ------------------------------------------------------------------------------
# Check cateogry quantifications  (MCA's coordinate)
# ------------------------------------------------------------------------------

plot(acci.hom, plot.type = "transplot")


acci.hom$quantifications$mode



# ------------------------------------------------------------------------------
# Symmetric map (joint plot)
# ------------------------------------------------------------------------------
# In the Princals applications, we are mostly interested in plotting the lodings.
# In Homals the most important plot is based on category quantifications, that is, a symmetric CA map which Gifi calls joint plot.
# In this plot we are interested in interpreting associations among the single item categories, associations among countries, and how the countries
# are associated with the item categories.
# Note that in Homals the same issues apply as in single and multiple CA when it comes to interpreting distances between categories of different items

par(mfrow = c(1,1))


plot(acci.hom, plot.type = "jointplot")
lines(acci.hom$quantifications$age, col = "gray", lty = 1)
lines(acci.hom$quantifications$result, col = "gray", lty = 2)
lines(acci.hom$quantifications$mode, col = "orange", lty = 1)
lines(acci.hom$quantifications$gender, col = "orange", lty = 2)


