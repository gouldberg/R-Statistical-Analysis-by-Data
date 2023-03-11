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
# Mean-Based Clustering:  kmeans()
#   - K-means clustering attempts to find groups that are most compact, in terms of the mean sum-of-squares deviation of each observation from
#     the multivariate center (centroid) of its assigned group.
#     K-means clustering relies on Euclidean distance and thus it is only appropriate for numeric data or data that can be reasonably coearced to numeric.
# ---------------------------------------------------------------------------

seg.df.num <- seg.df2 %>% mutate(gender = ifelse(gender == "Male", 0, 1), ownHome = ifelse(ownHome == "ownNo", 0, 1), subscribe = ifelse(subscribe == "subNo", 0, 1))

summary(seg.df.num)



# ----------
# assess the variances
sapply(seg.df.num, var)



# ----------
# We must standardize the data
( rge <- sapply(seg.df.num, function(x) diff(range(x))) )
seg.df.num_s <- sweep(seg.df.num, 2, rge, FUN = "/")
sapply(seg.df.num_s, var)




# ----------
# Assess the number of clusters by withinss
set.seed(96743)
n <- nrow(seg.df.num_s)
wss <- rep(0, 15)
wss[1] <- (n - 1) * sum(sapply(seg.df.num_s, var))

for(i in 2:15) wss[i] <- sum(kmeans(seg.df.num_s, centers = i)$withinss)

plot(1:15, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")




# ----------
set.seed(96743)

k <- 4
# seg.k <- kmeans(seg.df.num, centers = k)
seg.k <- kmeans(seg.df.num_s, centers = k)

table(seg.k$cluster)


plot(seg.df.num_s, col = seg.k$cluster)



# ---------------------------------------------------------------------------
# Mean-Based Clustering:  visualize dissimilarity matrix
# ---------------------------------------------------------------------------

d <- dist(scale(seg.df.num, center = FALSE))

library(lattice)

levelplot(as.matrix(d))




# ---------------------------------------------------------------------------
# Mean-Based Clustering:  kmeans():  check the clusters variation
# ---------------------------------------------------------------------------
# segment group statistics
seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


seg.summ(seg.df2, seg.k$cluster)


# -->
# Unlike with hclust() we now see some interesting differences; the groups appear to vary by age, gender, kids, income, and home ownership.



# ----------
seg.df2$cluster <- seg.k$cluster


library(lattice)

boxplot(seg.df.num$age ~ seg.k$cluster, ylab = "Age", xlab = "Cluster")
boxplot(seg.df.num$kids ~ seg.k$cluster, ylab = "Kids", xlab = "Cluster")
boxplot(seg.df.num$income ~ seg.k$cluster, ylab = "Income", xlab = "Cluster")

histogram(~ gender | cluster, data = seg.df2, layout = c(4,1), col = c("burlywood", "darkolivegreen"))
histogram(~ ownHome | cluster, data = seg.df2, layout = c(4,1), col = c("burlywood", "darkolivegreen"))



# ---------------------------------------------------------------------------
# Mean-Based Clustering:  kmeans():  Visualize the clusters
#   - clusplot() will perform dimensional reduction with principal components or multidimensional scaling as the data warrant,
#     and then plot the observations with cluster membership identified.
# ---------------------------------------------------------------------------

library(cluster)


# lines = 0: omit distance lines between groups
clusplot(seg.df.num_s, seg.k$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "K-means cluster plot")
# clusplot(seg.df2, seg.k$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "K-means cluster plot")


# -->
# We see that group 1 is modestly well differentiated, and has the highest average income.
# That may make it a good target for a potential comapaign.




# ---------------------------------------------------------------------------
# Mean-Based Clustering:  Visualize the clusters by Stripes plot and nNighbourhood plot
#   - Stripes plot is a effective way of visualizing the distance of each point from its closest and second closest cluster centroids
#   - flexclust::cclust() (Concex Clustering) performs k-means clustering, hard competitive learning or neural gas on a data matrix
# ---------------------------------------------------------------------------

library(flexclust)


ccl <- cclust(seg.df.num, k = 4, save.data = TRUE)
ccl_s <- cclust(seg.df.num_s, k = 4, save.data = TRUE)

summary(ccl)
summary(ccl_s)



# ----------
# Neighbourhood plot
plot(ccl, project = prcomp(seg.df.num), hull = TRUE, col = c("black", "red", "blue", "green"), xlab = "PC1", ylab = "PC2")
plot(ccl_s, project = prcomp(seg.df.num_s), hull = TRUE, col = c("black", "red", "blue", "green"), xlab = "PC1", ylab = "PC2")




# ----------
# Stripes plot
# type:  plot distance to closest, closest and second-closet or to all centroids ?
stripes(ccl, type = "second", col = 1)
stripes(ccl_s, type = "second", col = 1)
