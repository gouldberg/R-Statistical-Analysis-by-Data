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
# Correlation Networks:  Graphical Lasso
# ------------------------------------------------------------------------------

library(qgraph)


# 1. instead of using simple Pearson correlations, we use partial correlations.
# A partial correlation involving two nodes controls for the influence of all the remaining nodes in the network.
# Thus, the inferred edges are more reflective of direct influence among nodes.

# 2. instead of using a somewhat arbitrary correlation threshold or a Bonferroni corrected significance level, graphical lasso can be considered.


qgraph(cormat, layout = "spring", sampleSize = nrow(Rogers), graph = "glasso",
       groups = list(Depression = 1:16, OCD = 17:26),
       color = c("white", "gray"), labels = colnames(Rogers),
       title = "Depression / OCD Graphical Lasso")



# -->
# Note that, unlike in MDS, the distance between nodes in network plots has no meaning.
# The nodes are positioned such that the graph is produced in an aesthetically pleasing way.
# Therefore, scaling approaches such as MDS are highly illustrative within the context of correlation networks since they position the nodes
# according to their similarity. (i.e., high edge values in network slang)
