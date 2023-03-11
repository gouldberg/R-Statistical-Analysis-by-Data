# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------

load("./data/mite.RData")


dim(mite)
car::some(mite)


dim(mite.env)
car::some(mite.env)


dim(mite.xy)
car::some(mite.xy)




# ------------------------------------------------------------------------------
# Rarefaction analysis
#   - generally, one computes the number of expected species (one "rarefies") to a "sample size"
#     (remember that here this refers to the number of individuals) equal to the number of individuals of the sampling unit
#     where the abundance is the smallest.
# ------------------------------------------------------------------------------

# In this data set, most units (soil cores) contain 80 or more individuals, and only a handful contain fewer, with the minimum being 8.
# Therefore, let us rarefy the data set to a sample size of 80 individuals (and receive a warning because of that)
# Rarefaction to 80 individuals  --> modifies the ranking of the core

( mite.rare80 <- vegan::rarefy(mite, sample = 80) )



# ----------
# Compare ranking of observed and rarefied cores
sort(mite.nbsp)

sort(round(mite.rare80))




# ----------
# Sites with minimum and maximum estimated species richness
# The core with the smallest estimated number of species is core #67 (3.83) (as seen above, it is the core with the largest observed abundance)

mite.rare80[mite.rare80 == min(mite.rare80)]

mite.rare80[mite.rare80 == max(mite.rare80)]



# ----------
# Observed core with smallest predicted species richness
# Of the 781 individuals observed in that core, 723 belong to a single species (Limncfci)
# With 6 observied species for a total of 783 individuals, the estimation for 80 individuals drops to 3.8 species.

mite[which(mite.rare80 == min(mite.rare80)),]



# ----------
# Observed core with largest predicted species richness
# At the other extreme, the core with the highest estimated species richness is #30 with 19.1 species for 80 individuals
# Actual numbers of species and indivisuals are 21 species and 96 individuals  --> the estimated species richness for 80 individuals is close.

mite[which(mite.rare80 == max(mite.rare80)),]




# ------------------------------------------------------------------------------
# Rarefaction curve
#   - plot expected number of species as a function of the number of individuals
# ------------------------------------------------------------------------------

graphics.off();  par(mfrow=c(1,1));

# vegan::rarecurve(mite, step = 1, sample = 80, xlab = "Number of individuals (Sample Size)", ylab = "Species", label = TRUE, col = "blue")

vegan::rarecurve(mite[-67,], step = 1, sample = 80, xlab = "Number of individuals (Sample Size)", ylab = "Species", label = TRUE, col = "blue")



# -->
# Rarefaction curves for 69 of the 70 cores of the mite data.
# Core #67 was removed because it compressed all the other cores to the left to the graph
# label:  core numbers
# the vertical line corresponds to 80 individuas
# the horizontal lines give the number of expected species for each core and 80 individuals


