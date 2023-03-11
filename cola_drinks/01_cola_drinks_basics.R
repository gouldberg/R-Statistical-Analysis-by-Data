# setwd("//media//kswada//MyFiles//R//cola_drinks")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//cola_drinks")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coke Drinks
#   - A sample of psychology graduate students at the University of Florida mde blind, pairwise preference tests of 3 cola drinks,
#     Coke, Classic Coke and Pepsi
# ------------------------------------------------------------------------------

Cola <- read.table(file = "cola_drinks.dat", header = T, stringsAsFactors = FALSE, sep = "\t")


str(Cola)


Cola


# -->
# For 49 comparisons of Coke and Pepsi, Coke was preferred 29 times



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
