setwd("//media//kswada//MyFiles//R//perfume")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  perfume
#  - sensory data collected at Agrocampus of 98 consumers conducting categorization task using 12 luxury perfumes
#  - The participants were asked to divide the perfumes into groups according to their sensory similarities, and then to attribute a description to each of the groups.
# ------------------------------------------------------------------------------

perfume <- read.table("perfume.csv", header = TRUE, sep = ";", row.names = 1)

dim(perfume)

str(perfume)


perfume[,1:10]



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis Using Homals
#   - French Approach to multiple CA:  use a singular value decomposition on either the Burt matrix or the indicator matrix
#   - Dutch Approach (i.e., Homals) solves the multiple CA problem numerically, which offers a great amount of flexibility
# ------------------------------------------------------------------------------

library(Gifi)

per.hom <- homals(perfume)

summary(per.hom)



# -->
# variance accounted for 1st 2 dimensions is 57.54% (sligtly decreased from mjca: 67.6%)



# ------------------------------------------------------------------------------
# Check cateogry quantifications  (MCA's coordinate)
# ------------------------------------------------------------------------------

# Different participants
plot(per.hom, plot.type = "transplot", var.subset = c("J40", "J54"))
plot(per.hom, plot.type = "transplot", var.subset = c("J31", "J62"))


per.hom$quantifications$J40
per.hom$quantifications$J54



# ----------
# similar participants
plot(per.hom, plot.type = "transplot", var.subset = c("J40", "J6"))


per.hom$quantifications$J40
per.hom$quantifications$J6




# ------------------------------------------------------------------------------
# Symmetric map (joint plot)
# ------------------------------------------------------------------------------
# In the Princals applications, we are mostly interested in plotting the lodings.
# In Homals the most important plot is based on category quantifications, that is, a symmetric CA map which Gifi calls joint plot.
# In this plot we are interested in interpreting associations among the single item categories, associations among countries, and how the countries
# are associated with the item categories.
# Note that in Homals the same issues apply as in single and multiple CA when it comes to interpreting distances between categories of different items

par(mfrow = c(1,1))


# Different participants
plot(per.hom, plot.type = "jointplot", var.subset = c("J40", "J54", "J31", "J62"))
lines(per.hom$quantifications$J40, col = "gray", lty = 1)
lines(per.hom$quantifications$J54, col = "gray", lty = 2)
lines(per.hom$quantifications$J31, col = "orange", lty = 1)
lines(per.hom$quantifications$J62, col = "orange", lty = 2)



# ----------
# similar participants
plot(per.hom, plot.type = "jointplot", var.subset = c("J40", "J6"))
lines(per.hom$quantifications$J40, col = "gray")
lines(per.hom$quantifications$J6, col = "orange")


