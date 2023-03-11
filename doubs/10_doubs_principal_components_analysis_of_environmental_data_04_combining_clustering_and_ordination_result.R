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
# Combining clustering and ordination result
#  - Plot together cluster dendrogram and PCA biplot (scaling 1)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Hierarchical clustering and extract site scores on PC1 and PC2
# ------------------------------------------------------------------------------

# Clustering the objects using the environmental data:
# Euclidean distance after standardizing the variables, followed by Ward clustering
env.w <- hclust(dist(scale(env)), "ward.D")


# Cut the dendrogram to yield 4 groups
gr <- cutree(env.w, k = 4)

grl <- levels(factor(gr))



# ----------
# Extract the site scores, scaling 1
sit.sc1 <- scores(env.pca, display = "wa", scaling = 1)


head(sit.sc1)



# ------------------------------------------------------------------------------
# Plot combined results
# ------------------------------------------------------------------------------

# Cluster dendrogram and PCA biplot (scaling1) of the Doubs environmental data with overlaid clustering results
# Plot the sites with cluster symbols and colours (scaling 1)
graphics.off();  par(mfrow=c(1,2));

plot(env.w)

p <- plot(env.pca, display = "wa", scaling = 1, type = "n", main = "PCA correlation + clusters")
abline(v = 0, lty = "dotted");  abline(h = 0, lty = "dotted");


# position points by site scores (sit.sc1)
for (i in 1:length(grl)) points(sit.sc1[gr == i, ], pch = (14 + i), cex = 2, col = i + 1)
text(sit.sc1, row.names(env), cex = 0.7, pos = 3)


# add the dendrogram
vegan::ordicluster(p, env.w, col = "dark grey")

legend(locator(1), paste("Cluster", c(1:length(grl))), pch = 14 + c(1:length(grl)), col = 1 + c(1:length(grl)), pt.cex = 2)

