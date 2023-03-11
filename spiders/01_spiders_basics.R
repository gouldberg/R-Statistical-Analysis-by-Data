setwd("//media//kswada//MyFiles//R//spiders")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Spiders
#   - Oxbrough et al. (2005) investigated how spider communities change over forestation cycles in conifer and broadleaf plantations.
#     They identified environmental and structural features of the habitat that can be used as indicators of spider biodiversity.
#     Thirty-one plots were surveyed, each comprising 5 to 7 sampling sites separated by a minimum of 50 metres. Five pitfall traps were
#     placed in each site for 9 weeks from May through July. Traps were emptied at three week intervals.
#
#   - There are various statistical issues with the spider dataset, and we will not provide a full statistical analysis.
#     Instead we use segments of the data to demonstrate mixed effects modelling and generalized linear mixed modelling (GLMM).
# ------------------------------------------------------------------------------

Spiders <- read.table(file = "Spiders.txt", header = TRUE, dec = ".")


str(Spiders)



# ----------
# Some plots are dropped from the analysis
Spiders$fPlot <- factor(Spiders$Plot)

Spiders <- Spiders %>% filter(! fPlot %in% c("4", "9", "11", "14", "23"))

Spiders$fPlot <- as.factor(as.numeric(Spiders$fPlot))



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
