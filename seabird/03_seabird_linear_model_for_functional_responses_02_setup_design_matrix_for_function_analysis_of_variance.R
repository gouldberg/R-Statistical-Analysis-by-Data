setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  seabird
# ------------------------------------------------------------------------------
data("seabird", package = "fda")

str(seabird)

car::some(seabird)



# ------------------------------------------------------------------------------
# Create design matrix for functional analysis of variance
#
#   - The model:  yijk(t) = mu(t) + (-1)^i * alpha(t) + betaij(t) + eijk(t)
#     i = 1,2:  food groups
#     j = 1, ..., ni:  birds within a food group
#     k = 1,2:  sites
#     mu(t):  intercept or grand mean indicating the average trend for all birds
#     alpha(t):  the ime trend of the mean difference between the shellfish / mullusk-eating birds and the fish-eating birds
#     (-1)^i:  1 if an observation is for a shellfish/mullsk eater, and -1 otherwise
#     betaij(t):  time trends for each bird, but represent deviations from mu(t) that are specific to a food group
#
#   - The total number of equations is 28, being two blocks of 13 species plus one constraint for each food groups.
#     There are two blocks, one for each bay or observation site.
# ------------------------------------------------------------------------------

# After some preliminary analyses we determined that there was no contribution from either site or food*site interaction.
# Now we use a reduced model with only a feed effect, but we add bird effects, which were seen in the plot to be strong.
# Birds are nested within feed groups, and either their effects must sum to zero within each group,
# or we must designate a bird in each group as a baseline, and provide dummy variables for the remainder.
# We opt for the latter strategy.


# ----------
# The design matrix contains
#  - an intercept dummy variable
#  - a feed dummy variable
#  - dummy variables for birds
# excluding the second bird in each group, which turns out to be the each group's most abundant species, and which is designated as the
# baseline bird for that group.


# 15 columns for the intercept + diet + 13 bird species
# 26 rows for the 26 (species - bay) combinations
Zmat0 = matrix(0, 26, 15)



# Intercept or baseline effect
Intercept = rep(1,26)



# Crustacean/Mollusc feeding effect:  a contrast between the two groups
# 1 for a shellfish eater and -1 for a fish eater
foodindex = c(1,2,5,6,12,13)
( fooddummy = c(2 * rep(1:13 %in% foodindex, 2) - 1) )



# Bird effect, one for each species
birddummy = diag(rep(1,13))
( birdvarbl = rbind(birddummy, birddummy) )



# fill the columns of the design matrix
Zmat0[,1]    = Intercept
Zmat0[,2]    = fooddummy
Zmat0[,3:15] = birdvarbl



# ----------
# Two extra dummy observations are added to the functional data object for log counts, and two additional rows are added to
# the design matrix to force the bird effects within each diet group to equal 0.

birdfd3 = birdfd2
birdfd3$coefs = cbind(birdfd3$coefs, matrix(0, 19, 2))

Zmat = rbind(Zmat0, matrix(0,2,15))
Zmat[27, shellfishindex + 2]  = 1
Zmat[28, fishindex + 2] = 1



Zmat


