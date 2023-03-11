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
# Correlation Networks:  compute centrality
# ------------------------------------------------------------------------------

library(qgraph)


graphics.off()
centralityPlot(cornet, include = c("Betweenness", "Closeness", "Strength"))


# -->
# Degree centrality (strength):  number of edges connected with a node
# Closeness centrality:  how close one node is to all the other nodes based on the shortest paths
# Betweenness centrality:  Interactions between nodes depend on the other nodes who lie on the path between them (i.e., they "control" the interactions)


# -->
# Items that socre high in "Betweenness centrality" are potentially connector items between OCD and depression.
