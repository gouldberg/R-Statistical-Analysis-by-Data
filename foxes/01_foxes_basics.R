setwd("//media//kswada//MyFiles//R//foxes")

packages <- c("dplyr", "rethinking", "gpairs", "vcd")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  foxes
#   - The urban fox (Vulpes vulpes) is a successful exploiter of human habitat. Since urban foxes move in packs and defend territories,
#     data on habitat quality adn population density is also included.
#   - The variables:
#        - group: Number of the social group the individual fox belongs to
#        - avgfood: The average amount of foold avvailable in the territory
#        - groupsize: The number of foxes in the social group
#        - area: Size of the territory
#        - weight: Body weight of the individual fox
# ------------------------------------------------------------------------------
data("foxes", package = "rethinking")

d <- foxes

dim(d)

str(d)



# ------------------------------------------------------------------------------
# data:  basics
# ------------------------------------------------------------------------------
gpairs::gpairs(d, diag.pars = list(fontsize = 20))
