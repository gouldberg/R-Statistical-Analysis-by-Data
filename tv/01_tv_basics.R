setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
#  - The data on television viewership from Hartigan and Kleiner (1984), used to illustrate the method with data on a large sample of TV viewers
#    whose behavior had been recorded for the Neilsen ratings.
#    This data set contains sample television audience data from Neilsen Media Research for the week starting November 6, 1995.
# ------------------------------------------------------------------------------

data("TV", package = "vcdExtra")


TV


dim(TV)


str(TV)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

TV2 <- margin.table(TV, c(1,3))

TV2



