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



# ----------
# chord distance among sites (Q-mode):  Euclidean distance computed on site vectors normalized to length 1
spe.norm <- decostand(spe, "normalize")
spe.ch <- vegdist(spe.norm, "euc")
attr(spe.ch, "labels") <- rownames(spe)


# ----------
# for comparison, clusters by hierarchical clustering
spe.ch.ward <- hclust(spe.ch, method = "ward.D2")
k <- 4
spech.ward.g <- cutree(spe.ch.ward, k = k)




# ------------------------------------------------------------------------------
# Partitioning Around Medoids (PAM)
#  - PAM minimizes the sum of the dissimilarities of the observations to their closest medoids.
#  - PAM is a least-squares method when applied to non-Euclidean D functions
#  - PAM is robust in that it tends to converge to the same solution with a wide array of starting medoids for a given k value.
#  - cluster::pam() accepts raw data or dissimilarity matrices (an advantage over kmeans() since it broadens the choice of association measures)
#    and allows the choice of an optimal number of groups using the silhouette criterion
# ------------------------------------------------------------------------------

# Choice of the number of clusters
# Loop: obtain average silhouette widths (asw) for 2 to 28 clusters
asw <- numeric(nrow(spe))

for (k in 2:(nrow(spe) - 1)) asw[k] <- cluster::pam(spe.ch, k, diss = TRUE)$silinfo$avg.width


k.best <- which.max(asw)



# ----------
# Not very interesting result k = 2 with asw = 0.3841
graphics.off();  par(mfrow=c(1,1));

plot(1:nrow(spe), asw, type = "h", main = "Choice of the number of clusters", xlab = "k (number of clusters)", ylab = "Average silhouette width")

axis(1, k.best, paste("optimum", k.best, sep = "\n"), col = "red", font = 2, col.axis = "red")

points(k.best, max(asw), pch = 16, col = "red", cex = 1.5)



# ----------
# PAM for k = 4 clusters
spe.ch.pam <- pam(spe.ch, k = 4, diss = TRUE)

summary(spe.ch.pam)

spe.ch.pam.g <- spe.ch.pam$clustering

spe.ch.pam$silinfo$widths

