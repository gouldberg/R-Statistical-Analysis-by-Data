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
# Compare classifications
#  - Optimized k-means (Ward + k-means) is best
# ------------------------------------------------------------------------------

# Compare with classification from Ward clustering and from k-means
# PAM result differs markedly from those of the Ward and k-means clusterings
table(spe.ch.pam.g, spech.ward.g)

table(spe.ch.pam.g, spe.kmeans.g)


# Compare classifications from k-means and from optimized Ward clustering
table(spe.kmeans.g, spech.ward.gk)



# ------------------------------------------------------------------------------
# Compare silhouette profile for k = 4 groups
#  --> Optimized k-means (Ward + k-means) is best
# ------------------------------------------------------------------------------

sil_kmeans <- cluster::silhouette(spe.kmeans.g, spe.ch)

sil_pam <- cluster::silhouette(spe.ch.pam)

rownames(sil_kmeans) <- row.names(spe)

# rownames(sil_pam) <- row.names(spe)


# ----------
graphics.off();  par(mfrow = c(2, 2));

k <- 4

plot(sil_ward, main = "Silhouette plot - Chord - Ward.D2", cex.names = 0.8, col = 2:(k+1), nmax = 100)

plot(sil_kmeans, main = "Silhouette plot - k-means", cex.names = 0.8, col = 2:(k + 1), nmax = 100)

plot(sil_wardgk, main = "Silhouette plot - Ward & k-means", cex.names = 0.8, col = 2:(k + 1), nmax = 100)

plot(sil_pam, main = "Silhouette plot - PAM", cex.names = 0.8, col = 2:(k + 1), nmax = 100)



