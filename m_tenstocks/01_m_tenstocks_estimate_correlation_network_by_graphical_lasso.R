# setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_時系列\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
#   - monthly log returns of stocks for ten U.S. companies from January 2001 to December 2011 for 132 observations
#     The ten companies can roughly be classified into 3 industrial sectors, namely, semiconductor, pharmaceutical,
#     and investment banks
#   - The mean returns of the ten stocks are all close to 0, but the log returns have some negative skewness and high excess kurtosis.
#
#   - Semiconductor
#        - TXN:  Texas instru.
#        - MU:  Micron Tech.
#        - INTC:  Intel Corp.
#        - TSM:  Taiwan Semi.
#   - Pharmaceutical
#        - PFE:  Pfizer
#        - MRK:  Merck & Co.
#        - LLY:  Eli Lilly
#   - Investment Bank
#        - JPM:  JPMorgan hase
#        - MS:  Morgan Stanley
#        - GS:  Goldman Sachs
# ------------------------------------------------------------------------------


data <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(data)


dim(data)


car::some(data)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTS::MTSplot(data[,2:11])





# ------------------------------------------------------------------------------
# bivariate pairs plot
# ------------------------------------------------------------------------------


library(car)


formula <- ~ TXN + MU + INTC + TSM + PFE + MRK + LLY + JPM + MS + GS


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)




# ------------------------------------------------------------------------------
# Correlation Network
# ------------------------------------------------------------------------------


# correlation matrix
cormat <- cor(data[,2:11])



# ----------
library(qgraph)


# minimum = 0.2:  lower absolute correlation threshold to 0.2 (i.e., small-medium effect size)

cornet <- qgraph(cormat, layout = "spring", minimum = 0.2, graph = "cor",
                 groups = list(semi = c(1,2,3,4), medi = c(5,6,7), invb = c(8,9,10)),
                 color = c("white", "gray", "orange"), labels = colnames(data[,2:11]))



# -->
# The thicker an edge, the higher the correlation
# Edges with negative correlations are colored differently

# MRK is quite independent  --> smallest "strength"
# TSM, INTC is strongly correlated with others  --> large "strength" and "large closeness"

# JPM and INTC may control others  --> large "betweenness"



# ----------
# strength:  number of edgs connected with a node
# z-standardized centrality values

centralityPlot(cornet)


# How close one node is to all the other nodes based on the shortest paths
centralityPlot(cornet, include = "Closeness")


# Interactions betwen nodes depend on the other nodes who lie on the path betwen them (i.e., they "control" the interactions)
centralityPlot(cornet, include = "Betweenness")




# ------------------------------------------------------------------------------
# Correlation Network:  by Graphical Lasso
#   - instead of using simple Pearson correlations, we use PARTIAL correlation
#     partial correlation involving two nodes controls for the influence of all the remaining nodes in the network
#     thus, inferred edges are more reflective of direct influence among nodes
#
#   - instead of using a somewhat arbitrary correlation threshold or a Bonferroni corrected significance level,
#     graphical lasso can be considered
# ------------------------------------------------------------------------------


glanet <- qgraph(cormat, layout = "spring", sampleSize = nrow(data), graph = "glasso",
                 groups = list(semi = c(1,2,3,4), medi = c(5,6,7), invb = c(8,9,10)),
                 color = c("white", "gray", "orange"), labels = colnames(data[,2:11]))





