setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)



# ----------
# compute tetrachoric correlation matrix

library(psych)

tetcor <- tetrachoric(zarsub)

tetcor

tetcor$rho



# ------------------------------------------------------------------------------
# Person parameter
# ------------------------------------------------------------------------------

( zarppar <- person.parameter(fitrasch2) )



# -->
# Children who answered the same number of items correctly will get the same person parameter.

# If the person parameter and an item parameter coincide, this means that this person has a probability of 0.5 to score 1 on this particular item.



# ----------
head(zarppar$theta.table)




# ----------
# add person parameters to the data matrix

zareki$theta <- zarppar$theta.table[,1]


