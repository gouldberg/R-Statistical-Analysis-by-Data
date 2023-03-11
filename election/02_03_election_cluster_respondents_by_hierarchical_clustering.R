# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//election")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  election
# ------------------------------------------------------------------------------

library(poLCA)

data(election)

str(election)



# ----------
dat <- election




# ---------------------------------------------------------------------------
# Hierarchical Clustering:  hclust()
#   - hclust() is a distance-based algorithm that operates on a dissimilarity matrix, and N-by-N matrix that
#     reports a metric for the distance between each pair of observations
#   - The hierarchical clustering method begins with each observations in itw own cluster.
#     It then successively joins neighboring observations or clusters one ata time according to their distances from one another, and continues this
#     until all observations are linked. (agglomerative method)
# ---------------------------------------------------------------------------
# The daisy() function in the cluster package works with mixed data types by rescaling the values, so we can use that instead of Euclidean distance
# A generalization of Gower's formula is used.

library(cluster)

nrow(dat[complete.cases(dat),])

dat_comp <- dat[complete.cases(dat),]


dat.dist <- daisy(dat_comp)

summary(dat.dist)



# ----------
# clustering
dat.hc <- hclust(dat.dist, method = "complete")


plot(dat.hc)



# ----------
# It is helpful to zoom in on one section of the chart.
# Cut it at a certain height and select the resulting branch that we want
plot(cut(as.dendrogram(dat.hc), h = 0.5)$lower[[1]])


# we can check the similarity of observations by selecting a few rows.
dat[c(471, 1473), ]




# ---------------------------------------------------------------------------
# Check the goodness-of-fit metric
#   - Cophenetic correlation coefficient (CPCC):  assesses how well a dendrogram matches the true distance metric (seg.dist)
# ---------------------------------------------------------------------------

cor(cophenetic(dat.hc), dat.dist)


# -->
# CPCC > 0.5 indicats a weak fit, meaning that the hierarchical tree represents the distances between respondents not well.



# ---------------------------------------------------------------------------
# Get specific segment assignemnts and check the group statistics
# ---------------------------------------------------------------------------

plot(dat.hc)

rect.hclust(dat.hc, k = 5, border = "red")



# ----------
# membership vector for 5 goups

dat.hc.segment <- cutree(dat.hc, k = 5)

table(dat.hc.segment)


# -->
# groups 1 and 2 dominate the assignment.











