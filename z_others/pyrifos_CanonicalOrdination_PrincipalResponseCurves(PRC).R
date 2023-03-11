# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  pyrifos
#  - The abundances of 178 invertebrate species (macroinvertebrates and zooplankton) subjected to insecticide treatments in asuatic mesocosms ("ditches")
#  - The species data are log-transformed abundances y = ln(10*y + 1)
#  - The experiment involved 12 mesocosms, which were surveyed on 11 occasions, so n = 12 * 11 = 132.
#  - 4 mesocosms served as controls (dose = 0) and the remaining 8 were treated once with the insecticide chlorpyrifos, with dose levels of 0.1, 0.9, 6 and 44 mugL^-1 in 2 mesocosms each.
#  - Therefore, the explanatory factor has  levels (the 4 dose levels, excluding control) and the covariate "week" has 11 levels.
#
#  - Chlorpyrifos experiment and experimental design :  Pesticide treatment in ditches (replicated) and followed over from 4 weeks before to 24 weeks after exposure 
#
# ------------------------------------------------------------------------------
# Extract the data (available in vegan)
data(pyrifos)
dim(pyrifos)

car::some(pyrifos)
head(pyrifos)



# ------------------------------------------------------------------------------
# Preparation
# ------------------------------------------------------------------------------
# Create factors for time (week) and treatment (dose). Create an additional factor "ditch" representing the mesocosm, for testing purposes
# gl():  n (number of levels) and k (number of replications)
( week <- gl(n = 11, k = 12, labels = c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)) )
( dose <- factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)) )
( ditch <- gl(12, 1, length = 132) )



# ------------------------------------------------------------------------------
# Principal Response Curves (PRC)
#
#  - Principal Response Curves address the problems related to the analysos of multivariate results of designed experiments that involve repeated measurements over time
#   by means of a modified form of RDA.
#  - The PRC method focuses on the differences between control and treatments; it provides a clear graphical illustration of treatment effects at the community as well as the species level.
#  - The PRC method specifically focuses on the difference between control and treatment level at each time point. To achieve that, one must remove the overall effect of time,
#    i.e., use the time factor as a covariable.
#  - The canonical coefficients resulting from the RDA are plotted against time;  curves representing these scores for each treatment along time are called the PRC of the community.
#  - A high positive weight indicates a high likelihood that the species follows the overall pattern of the PRC. A negative weights shows a tendency of the species to follow an opposite pattern.
#    A weight close to 0 indicates either no pattern or a pattern of a shape differing from the overall one.
#  - Note that a small weight would not indicate a lack of response of the species to the treatment, but a response pattern that may be strong but different to the overall (community-level) one.
# ------------------------------------------------------------------------------
# PRC
mod <- vegan::prc(pyrifos, dose, week)
mod            # Modified RDA
summary(mod)   # Results formatted as PRC



# PRC plot; at the right of it, only species with large total (log-transformed) abundances are reported
# The lines represent differences with respect to the control plots ("dose" = 0) expressed as casnonical coefficients on the first RDA axis.
# Along the righ-hand margin, species weights show the degree of agreement between the species-level response to the treatment and the overall, communitiy-level response.
logabu <- colSums(pyrifos)
graphics.off();  par(mfrow=c(1,1));
plot(mod, select = logabu > 200)


# --> The intensity of community response to the treatment clearly dependes on the chlorpyrifos dose.
# At higher doses, the response is greater and lasts longer.
# Note, that between does 6 and 44 the peai intensities of the responses are comparable, the difference is that dose 44 produces a longer effect.


# Statistical test
# Ditches are randomized, we have a time series, and are only interested in the first axis
ctrl <- permute::how(plots = Plots(strata = ditch, type = "free"), within = Within(type = "series"), nperm = 999)
anova(mod, permutations = ctrl, first = TRUE)

