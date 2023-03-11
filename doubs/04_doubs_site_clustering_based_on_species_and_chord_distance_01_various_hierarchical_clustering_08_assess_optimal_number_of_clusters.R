setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]




# ------------------------------------------------------------------------------
# Appropriate number of groups
#  - 3 methods:  Average Silhouette Widths, matrix comparison, and diagnostic species.
#
# Select a dendrogram (Ward/chord) and apply three criteria to choose the optimal number of clusters
# ------------------------------------------------------------------------------


# Choose and rename the dendrogram ("hclust" object)
hc <- spe.ch.ward
# hc <- spe.ch.beta2
# hc <- spe.ch.complete



# ------------------------------------------------------------------------------
# 1. Average silhouette widths (Rousseeuw quality index)
#  - silhouette width is a measure of the degree of membership of an object to its cluster, based on the average dissimilarity between this object and all objects of the cluster
#    to which it belongs, compared to the same measure computed for the next closet cluster. Ranging from -1 to 1 and can be averaged over all objects of a partition
#  - At each fusion level, the average silhouette width can be used as a measure of the quality of the partition
# ------------------------------------------------------------------------------

Si <- numeric(nrow(spe))

for (k in 2:(nrow(spe) - 1))
{
  sil <- cluster::silhouette(cutree(hc, k = k), spe.ch)
  Si[k] <- summary(sil)$avg.width
}



# ------------------------------------------------------------------------------
# 2. Comparing between the dissimilarity matrix and binary matrices computed from the dendrogram cut at various levels
#  - Optimal number of clusters according to matrix correlation statistic (Pearson)
# ------------------------------------------------------------------------------

# Function to compute a binary dissimilarity matrix from clusters
grpdist <- function(X){
  require(cluster)
  gr <- as.data.frame(as.factor(X))
  distgr <- cluster::daisy(gr, "gower")
  distgr
}


kt <- data.frame(k = 1:nrow(spe), r = 0)

for (i in 2:(nrow(spe) - 1)) 
{
  gr <- cutree(hc, i)
  distgr <- grpdist(gr)
  mt <- cor(spe.ch, distgr, method = "pearson")
  kt[i, 2] <- mt
}



# ------------------------------------------------------------------------------
# 3. Optimal number of clusters according as per indicator species fidelity analysis (IndVal, Dufrene-Legendre; package: labdsv)
#  - The basic idea is to retain clusters that are best characterized by a set of diagnostic species, also called "indicator", "typical", "characteristic", or "differential" species,
#    i.e. species that are significantly more frequent and abundant in a given group of sites.
#  - The best partition would be the one that maximizes both (i) the sum of indicator values and (ii) the proportion of clusters with significant indicator species.
#  - We use index IndVal (Dufrene and legendre), which integrates a specificity and a fidelity measure
# ------------------------------------------------------------------------------

IndVal <- numeric(nrow(spe))

ng <- numeric(nrow(spe))

for (k in 2:(nrow(spe) - 1))
{
  iva <- labdsv::indval(spe, cutree(hc, k = k), numitr = 1000)
  gr <- factor(iva$maxcls[iva$pval <= 0.05])
  ng[k] <- length(levels(gr)) / k
  iv <- iva$indcls[iva$pval <= 0.05]
  IndVal[k] <- sum(iv)
}



# ----------
k.best <- which.max(IndVal[ng == 1]) + 1


col3 <- rep(1, nrow(spe))

col3[ng == 1] <- 3



# ------------------------------------------------------------------------------
# Plot all indicators by number of clusters
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(2, 2))


# ----------
# Average silhouette widths (Rousseeuw quality index)
k.best <- which.max(Si)
plot(1:nrow(spe), Si, type = "h", main = "Silhouette-optimal number of clusters", xlab = "k (number of clusters)", ylab = "Average silhouette width")
axis(1, k.best, paste("optimum", k.best, sep = "\n"), col = "red", font = 2, col.axis = "red")
points(k.best, max(Si), pch = 16, col = "red", cex = 1.5)



# ----------
# Comparing between the dissimilarity matrix and binary matrices computed from the dendrogram cut at various levels
#  - Optimal number of clusters according to matrix correlation statistic (Pearson)
k.best <- which.max(kt$r)
plot(kt$k, kt$r, type = "h", main = "Matrix correlation-optimal number of clusters", xlab = "k (number of clusters)", ylab = "Pearson's correlation")
axis(1, k.best, paste("optimum", k.best, sep = "\n"), col = "red", font = 2, col.axis = "red")
points(k.best, max(kt$r), pch = 16, col = "red", cex = 1.5)



# ----------
# IndVal
# significant indicator species in all k clusters are highlighted in green
k.best <- which.max(IndVal[ng == 1]) + 1
col3 <- rep(1, nrow(spe))
col3[ng == 1] <- 3
plot(1:nrow(spe), IndVal, type = "h", main = "IndVal-optimal number of clusters", xlab = "k (number of clusters)", ylab = "IndVal sum", col = col3)
axis(1, k.best, paste("optimum", k.best, sep = "\n"), col = "red", font = 2, col.axis = "red")
points(which.max(IndVal), max(IndVal), pch = 16, col = "red", cex = 1.5)
text(28, 15.7, "a", cex = 1.8)



# ----------
plot(1:nrow(spe), ng, type = "h", xlab = "k (number of clusters)", ylab = "Ratio", main = "Proportion of clusters with significant indicator species", col = col3)
axis(1, k.best, paste("optimum", k.best, sep = "\n"), col = "red", font = 2, col.axis = "red")
points(k.best, max(ng), pch = 16, col = "red", cex = 1.5)
text(28, 0.98, "b", cex = 1.8)



# Only the partition in two clusters, simply contrasting the upstream and downstream sites, fully meets the 2 criteria.
# However, a partition in 3 or 4 clusters, allowing the discovery of more subtle structures,
# would be an acceptable choice despite the absence of positive differential species
# (a species that is more frequency in a given group than in others) for some groups

