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
# latlong <- latlong[-8,]


# ----------
# data preparation
dfs <- env[, 1]
env2 <- env[, -1]
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
env3 <- env2
env3$slo <- slo2
envtopo <- env2[, c(1 : 3)]
envchem <- env2[, c(4 : 10)]



# ------------------------------------------------------------------------------
# Interactive 3-d plots for CCA results
# plot of the sites only (wa scores)
# ------------------------------------------------------------------------------

library(vegan3d)


# plot of the sites only (wa scores)

ordirgl(spe.cca.pars, type = "t", scaling = 1)



# ------------------------------------------------------------------------------
# Interactive 3-d plots for CCA results
# Connect weighted average scores to linear combination scores
# ------------------------------------------------------------------------------

ordirgl(spe.cca.pars, scaling = 1, col = "purple")


# -->
# the purple connections show how well the CCA model fits the data.
# The shorter the connections, the better fit.



# ------------------------------------------------------------------------------
# Interactive 3-d plots for CCA results
# plot the sites (wa scores) with a clusteirng result
# Colour sites according to cluster membership
# ------------------------------------------------------------------------------

gr <- cutree(hclust(vegdist(spe.hel, "euc"), "ward.D2"), 4)


# connect sites to cluster centrokds
ordirgl(spe.cca.pars, type = "t", scaling = 1, ax.col = "black", col = gr + 1)
orglspider(spe.cca.pars, gr, scaling = 1)



# -->
# The sitesa are nicely clustered along their major ecological gradients.
# Remember that this is an analysis of the fish community data



# ------------------------------------------------------------------------------
# Interactive 3-d plots for CCA results
# Complete CCA 3D triplot
# ------------------------------------------------------------------------------

ordirgl(spe.cca.pars, type = "t", scaling = 2)
orgltext(spe.cca.pars, display = "species", type = "t", scaling = 2, col = "cyan")



# ----------
# plot species groups (Jaccard dissimilarity, useable in R mode)

gs <- cutree(hclust(vegdist(t(spe), method = "jaccard"), "ward.D2"), k = 4)


ordirgl(spe.cca.pars, display = "species", type = "t", col = gs + 1)








