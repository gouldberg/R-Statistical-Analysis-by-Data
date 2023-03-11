setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)



# ----------
# convert table to case form
library(tidyverse)

TV_c <- expand.dft(as.data.frame(as.table(TV)))

head(TV_c)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis Using Homals
#   - French Approach to multiple CA:  use a singular value decomposition on either the Burt matrix or the indicator matrix
#   - Dutch Approach (i.e., Homals) solves the multiple CA problem numerically, which offers a great amount of flexibility
# ------------------------------------------------------------------------------

library(Gifi)

tv.hom <- homals(TV_c)

summary(tv.hom)



# -->
# variance accounted for 1st 2 dimensions is 40.42%
# (slightly increased from mjca() !!)



# ------------------------------------------------------------------------------
# Check cateogry quantifications  (MCA's coordinate)
# ------------------------------------------------------------------------------

plot(tv.hom, plot.type = "transplot")



# -----------
tv.hom$quantifications$Day

tv.hom$quantifications$Time

tv.hom$quantifications$Network



# ------------------------------------------------------------------------------
# Symmetric map (joint plot)
# ------------------------------------------------------------------------------
# In the Princals applications, we are mostly interested in plotting the lodings.
# In Homals the most important plot is based on category quantifications, that is, a symmetric CA map which Gifi calls joint plot.
# In this plot we are interested in interpreting associations among the single item categories, associations among countries, and how the countries
# are associated with the item categories.
# Note that in Homals the same issues apply as in single and multiple CA when it comes to interpreting distances between categories of different items

par(mfrow = c(1,1))


plot(tv.hom, plot.type = "jointplot")
lines(tv.hom$quantifications$Day, col = "gray", lty = 1)
lines(tv.hom$quantifications$Time, col = "gray", lty = 2)
lines(tv.hom$quantifications$Network, col = "orange", lty = 1)


