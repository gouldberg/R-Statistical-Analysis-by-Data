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
# Model-Based Clustering:  Mclust()
#   - A limitation of k-means analysis that it requires specifying the number of clusters.
#   - The key idea for model-based clustering is that observations come from groups with different statistical distributions (such as different means and variances)
#     The algorithms try to find the best set of such underlying distributions to explain the observed data.
#     Such models are alsl known as "mixture models" because it is assumed that the data reflect a mixture of observations drawn from different populations,
#   - mclust() models clusters as being drawn from a mixture of normal distribution and it uses only numeric data.
# ---------------------------------------------------------------------------

seg.df.num <- seg.df2 %>% mutate(gender = ifelse(gender == "Male", 0, 1), ownHome = ifelse(ownHome == "ownNo", 0, 1), subscribe = ifelse(subscribe == "subNo", 0, 1))

summary(seg.df.num)



# ----------
library(mclust)

seg.mc <- Mclust(seg.df.num)

summary(seg.mc)


# -->
# The data are estimated to have 3 clusters
# Mclust() compared a varietty of different mixture shapes and concluded that an ellipsoidal model (modeling the data as multivariate ellipses) fit best.



# ----------
# We try 4-cluster solution and compare
seg.mc4 <- Mclust(seg.df.num, G = 4)

summary(seg.mc4)


# -->
# Forcing it to find 4 clusters resulted in quite a different model with lower log-likelihood, a different multivariate pattern (diagonal),
# and no obvious correspondence in the cluster table



# ---------------------------------------------------------------------------
# Compare models with BIC()
# ---------------------------------------------------------------------------

BIC(seg.mc, seg.mc4)



# ---------------------------------------------------------------------------
# check the clusters variation
# ---------------------------------------------------------------------------
# segment group statistics
seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


seg.summ(seg.df2, seg.mc$class)


# -->
# Unlike with hclust() we now see some interesting differences; the groups appear to vary by age, gender, kids, income, and home ownership.



# ----------
seg.df2$cluster <- seg.mc$class


library(lattice)

boxplot(seg.df.num$age ~ seg.k$cluster, ylab = "Age", xlab = "Cluster")
boxplot(seg.df.num$kids ~ seg.k$cluster, ylab = "Kids", xlab = "Cluster")
boxplot(seg.df.num$income ~ seg.k$cluster, ylab = "Income", xlab = "Cluster")

histogram(~ gender | cluster, data = seg.df2, layout = c(4,1), col = c("burlywood", "darkolivegreen"))
histogram(~ ownHome | cluster, data = seg.df2, layout = c(4,1), col = c("burlywood", "darkolivegreen"))



# ---------------------------------------------------------------------------
# Visualize the clusters
#   - clusplot() will perform dimensional reduction with principal components or multidimensional scaling as the data warrant,
#     and then plot the observations with cluster membership identified.
# ---------------------------------------------------------------------------

library(cluster)


# lines = 0: omit distance lines between groups
clusplot(seg.df2, seg.mc$class, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Model-based cluster plot")

clusplot(seg.df2, seg.mc4$class, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Model-based cluster plot, G=4")







