# setwd("//media//kswada//MyFiles//R//doubs")
setwd("//media//kswada//MyFiles//R//Multivariate\statistics_analysis//doubs")

packages <- c("dplyr", "vegan", "RgoogleMaps", "googleVis", "gclus")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ------------------------------------------------------------------------------
# Site - Species data:  Abundances, Occurrences and species relative frequencies
#  - Verneaux used a semi-quantitative, species-specific abundance scale (0-5), so that comparison between species abundances will make sense.
#  - The maximum value, 5, corresponds to the class with the maximum number of individuals captured by electrical fishing in the Doubs River and its tributaries (not only in this data set)
#    Therefore, species-specific codes cannot be understood as unbiased estimates of the true abundances (number or density of individuals) or biomasses at the sites.
# ------------------------------------------------------------------------------

car::some(spe)


# -->
# Note the abundance is measure 0-5 scale, 
# herefore rarefaction analysis is not appplied for this data. 



# ------------------------------------------------------------------------------
# Ranges of species-specific abundance scale
# ------------------------------------------------------------------------------


range(spe)

apply(spe, 2, range)



# -->
# Some species have lower abundance at some points



# ------------------------------------------------------------------------------
# Site * species distrituion (all species confounded)
# ------------------------------------------------------------------------------

( ab <- table(unlist(spe)) )

graphics.off();  par(mfrow=c(1,1));

barplot(ab, las = 1, xlab = "Abundance class", ylab = "Frequency", col = gray(5:0 / 5))



# ------------------------------------------------------------------------------
# Zero abundance
# ------------------------------------------------------------------------------

# 435 site * species, where site * species== 0 

sum(spe == 0)



# Proportion of zeros in the community data set:  High frequency of zero (53.7%)
sum(spe == 0) / (nrow(spe) * ncol(spe))




# ------------------------------------------------------------------------------
# the number of sites where each species is present
# ------------------------------------------------------------------------------

spe.pres <- apply(spe > 0, 2, sum)

sort(spe.pres)



# ------------------------------------------------------------------------------
# Species relative frequency
# ------------------------------------------------------------------------------

spe.relf <- 100 * spe.pres/nrow(spe)


round(sort(spe.relf), 1)


# -->
# Sqce (Europearn Chub) is found at 83.3% of sites (25)
# Icme (Black bullhead) is found at only 23.3% of sites (7)




# ----------
# Species occurrence:  12 species are found at 10-14 sites.
graphics.off()
par(mfrow = c(1,2))

hist(spe.pres, main = "Species Occurrences", right = FALSE, las = 1, xlab = "Number of occurrences", ylab = "Number of species", breaks = seq(0, 30, by = 5), col = "bisque")

hist(spe.relf, main = "Species Relative Frequencies", right = FALSE, las = 1, xlab = "Frequency of occurrences (%)", ylab = "Number of species", breaks = seq(0, 100, by = 10), col = "bisque")




