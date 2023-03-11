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
# Which is the robust groups of sites ?:  assess the uncertainty (or robustness) of a classification
#  - This has been done abundantly in phylogenetic analysis
#
# Multiscale Bootstrap Resampling
#  - randomply sampling subsets of the data and computing the clustering on these subsets.
#  - counts the proportion of the replicate clustering results where a given cluster appears. This proportion is called the bootstrap probability (BP) of the cluster.
#  - Multiscale bootstrap resampling has been developed as an enhancement. In this method bootstrap samples of several different sizes are used to estimate the p-value of the each cluster.
#  - This produces "approximately unbised" (AU) p-values.
#
# pvclust packages
#  - provides functions to plot a dendrogram with bootstrap p-values associated to each cluster.
#  - AU p-values are printed in red, the less accurate BP values are printed in green, clusters with high AU values can be considered as strongly supported by the data.
#  - Note that in pvclust() the data object must be transposed with respect to our usual lyaout (rows are variable)
# ------------------------------------------------------------------------------
# Compute p-values for all clusters (edges) of the dendrogram
# Beware:  in function pvclust() the data object must be transposed with respect to usual layout (rows are variables)
# parallel = TRUE is used to greatly accelerate the computation with large data sets. It calls upon package parallel

spech.pv_ward <- pvclust(t(spe.norm), method.hclust = "ward.D2", method.dist = "euc", parallel = TRUE)



# ----------
# plot the AU (approximately unbiased) p-values and BP (bootstrap probability) on dendrogram
graphics.off();  par(mfrow=c(1,1));
plot(spech.pv_ward);  pvrect(spech.pv_ward, alpha = 0.95, pv = "au");  lines(spech.pv_ward);  pvrect(spech.pv_ward, alpha = 0.91, border = 4)


# Clusters enclosed in red boxes and underlined correspond to significant AU p-values (p >= 0.95)
# whereas clusters enclosed in blue rectangles show less significant values (p > 0.91 in this example)

# --> it seems that 4 groups is NOT robust enough ....



# ------------------------------------------------------------------------------
# Try other clustering method
# ------------------------------------------------------------------------------

spech.pv_ave <- pvclust(t(spe.norm), method.hclust = "average", method.dist = "euc", parallel = TRUE)

spech.pv_sgl <- pvclust(t(spe.norm), method.hclust = "single", method.dist = "euc", parallel = TRUE)

spech.pv_comp <- pvclust(t(spe.norm), method.hclust = "complete", method.dist = "euc", parallel = TRUE)



# ----------
graphics.off();  par(mfrow=c(2,2));

plot(spech.pv_ward);  pvrect(spech.pv_ward, alpha = 0.95, pv = "au");  lines(spech.pv_ward);  pvrect(spech.pv_ward, alpha = 0.91, border = 4)

plot(spech.pv_ave);  pvrect(spech.pv_ave, alpha = 0.95, pv = "au");  lines(spech.pv_ave);  pvrect(spech.pv_ave, alpha = 0.91, border = 4)

plot(spech.pv_sgl);  pvrect(spech.pv_sgl, alpha = 0.95, pv = "au");  lines(spech.pv_sgl);  pvrect(spech.pv_sgl, alpha = 0.91, border = 4)

plot(spech.pv_comp);  pvrect(spech.pv_comp, alpha = 0.95, pv = "au");  lines(spech.pv_comp);  pvrect(spech.pv_sgl, alpha = 0.91, border = 4)



