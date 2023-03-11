setwd("//media//kswada//MyFiles//R//rogers")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rogers
# ------------------------------------------------------------------------------

data("Rogers", package = "MPsychoR")

str(Rogers)

dim(Rogers)

car::some(Rogers)



# ------------------------------------------------------------------------------
# Correlation Networks based on simple Pearson Correlation
# ------------------------------------------------------------------------------

library(qgraph)


cormat <- cor(Rogers)


# set lower absolute correlation threshold to 0.2 (i.e., small-medium effect size).
cornet <- qgraph(cormat, layout = "spring", minimum = 0.2, graph = "cor", groups = list(Depression = 1:16, OCD = 17:26),
                 color = c("white", "gray"), labels = colnames(Rogers),
                 title = "Depression/OCD Correlation Network")


# -->
# The thicker an edge, the higher the correlation
# Edges with negative correlations are colored differently.  (green lines: positive correlations and red lines negative correlations)

# There are two densely connected groups of items:
# all OCD items and a bunch depression items except those in the periphery related to sleep and appetite.

# Based on such a correlation network, we have the option to compute various typical network measures to characterize each symptom's role in the network,
# such as centrality






