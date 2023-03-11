setwd("//media//kswada//MyFiles//R//stylo")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stylo
#   - Variables
#        - word:  number of times a word appears in a single text
#        - freq:  frequency of the number of times a word appears in a text
# ------------------------------------------------------------------------------
data("stylo", package = "gamlss.data")


str(stylo)

car::some(stylo)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

# MASS::truehist(stylo$word, nbins = 200)

MASS::truehist(stylo$freq, nbins = 200)


