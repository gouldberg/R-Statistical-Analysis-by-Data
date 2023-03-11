# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//consumer_segment")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Consumer Segment Data (created data)
# ------------------------------------------------------------------------------

seg.df <- read.csv("rintro-chapter5.csv", header = TRUE)


str(seg.df)


Hmisc::describe(seg.df)

summary(seg.df)

car::some(seg.df)


# remove the known segment assignments
seg.df2 <- seg.df[, -7]


# 300 * 6
dim(seg.df2)



# ---------------------------------------------------------------------------
# Hierarchical Clustering:  hclust()
#   - hclust() is a distance-based algorithm that operates on a dissimilarity matrix, and N-by-N matrix that
#     reports a metric for the distance between each pair of observations
#   - The hierarchical clustering method begins with each observations in itw own cluster.
#     It then successively joins neighboring observations or clusters one ata time according to their distances from one another, and continues this
#     until all observations are linked. (agglomerative method)
# ---------------------------------------------------------------------------


# If we do not care about the factor variables, then we could compute Euclidean distance using only the numeric columns
d <- dist(seg.df2[, c("age", "income", "kids")])

as.matrix(d)[1:5, 1:5]


# -->
# As expected, the distance matrix is symmetric, and the distance of an observation from itself is zero.



# ----------
# The daisy() function in the cluster package works with mixed data types by rescaling the values, so we can use that instead of Euclidean distance
# A generalization of Gower's formula is used.

library(cluster)

seg.dist <- daisy(seg.df2)

summary(seg.dist)



# ----------
# clustering
seg.hc <- hclust(seg.dist, method = "complete")

plot(seg.hc)



# ----------
# It is helpful to zoom in on one section of the chart.
# Cut it at a certain height and select the resulting branch that we want
plot(cut(as.dendrogram(seg.hc), h = 0.5)$lower[[1]])


# we can check the similarity of observations by selecting a few rows.
seg.df[c(101, 107), ]




# ---------------------------------------------------------------------------
# Check the goodness-of-fit metric
#   - Cophenetic correlation coefficient (CPCC):  assesses how well a dendrogram matches the true distance metric (seg.dist)
# ---------------------------------------------------------------------------

cor(cophenetic(seg.hc), seg.dist)


# -->
# CPCC > 0.7 indicats a relatively strong fit, meaning that the hierarchical tree represents the distances between customers well.



# ---------------------------------------------------------------------------
# Get specific segment assignemnts and check the group statistics
# ---------------------------------------------------------------------------

plot(seg.hc)

rect.hclust(seg.hc, k = 4, border = "red")



# ----------
# membership vector for 4 goups

seg.hc.segment <- cutree(seg.hc, k = 4)

table(seg.hc.segment)


# -->
# groups 1 and 2 dominate the assignment.


# ----------
# segment group statistics
seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


seg.summ(seg.df2, seg.hc.segment)


# -->
# We see that groups 1 and 2 are distinct from 3 and 4 due to subscription status.
# Among those who do not subscribe, group 1 is all male while group 1 is all female.
# Subscribers are differentiated into those who own home (group3) or not (group4)



# ---------------------------------------------------------------------------
# Check the hypothesis for important factors for segmentation by plotting
# ---------------------------------------------------------------------------

plot(jitter(as.numeric(seg.df2$gender)) ~ jitter(as.numeric(seg.df2$subscribe)), col = seg.hc.segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(1, at = c(1,2), labels = c("Subscribe: No", "Subscribe: Yes"))
axis(2, at = c(1,2), labels = levels(seg.df2$gender))


# -->
# We see clearly that the non-subscribes are broken into two segments (colored red and black) that are perfectly correlated with gender.
# This is not interesting results ...

# The scaling in daisy() rescales variables to [0,1] and this will make two-category factors more influential ...









