# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr", "ape", "spdep", "ade4", "adegraphics", "adespatial", "vegan")
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



source("./functions/plot.links.R")
source("./functions/sr.value.R")
source("./functions/quickMEM.R")
source("./functions/scalog.R")



# ------------------------------------------------------------------------------
# detrending the data by 1st degree function of the site geographical coordinates
# ------------------------------------------------------------------------------

# transform the data
mite.h <- decostand(mite, "hellinger")


head(as.matrix(mite.h))



# ----------
# Is there a linear trend in the mite data ?

anova(rda(mite.h, mite.xy))


# -->
# significant trend



# ----------
# The species data are first detrended to make them second-order stationary
# second-order stationary
#  - To test the significance of coefficients of spatial correlation, the condition of second-order stationarity must be met.
#  - That condition states that the mean of the variable and its spatial covriance are the same over the study area, and that its variance is finite.
#  - This condition is that the spatial variation of the data should be adequately described by the same single spatial correlation function in all portions of the study area.


tmpmod <- lm(as.matrix(mite.h) ~ ., data = mite.xy)


summary(tmpmod)


mite.h.det <- resid(tmpmod)


dim(mite.h.det)




# ------------------------------------------------------------------------------
# Mantel correlogram of the oribatid mite data
# ------------------------------------------------------------------------------

# Compute a standardized Mantel statistic rM (analogous to a Pearson's r coefficient) between a dissimilarity matrix among sites
# and a matrix where pairs of sites belonging to the same distance class receive value 0 and the other pairs, value 1.
# The process is repeated for each distance class. Each rM value can be test for by permutations.
# The expectation of the Mantel statistic for no spatial correlation is rM = 0

# dissimilarity matrix among sites (detrended data)
mite.h.D1 <- dist(mite.h.det)



# ----------
# Compute a standardized Mantel statistic rM (analogous to a Pearson's r coefficient) between a dissimilarity matrix among sites
( mite.correlog <- mantel.correlog(mite.h.D1, XY = mite.xy, nperm = 999) )




# ----------
# Number of classes:  computed using Sturge's rule = 1 + (3.3219 * log10(n)),
# here the number of pairwise distances
# or: mite.correlog[2]
mite.correlog$n.class



# ----------
# Break points
# or: mite.correlog[3]
mite.correlog$break.pts




# ----------
# Mantel correlogram of the Hellinger-transformed and detrended oribatid mite species data.
plot(mite.correlog)



# -->
# Black squares:  significant multivariate spatial correlation after Holm correction for multiple testing
# The abscissa is labelled in metres since this is the unit of the data used to construct the distance class.

# significant positive spatial correlation in the first 2 distance classes (between 0.15 m and 1.61 m, see break points)
# and negative significant correlation in the fourth to sixth classes (between 2.34 and 4.52 m).

# Close sites tend to show similar communities because the soil conditions are rather similar.
# On the other hand, any pair of sites whose members are about 2-4m apart falls into contrasting soil conditions,
# which in turn explains why their mite communities are different.



