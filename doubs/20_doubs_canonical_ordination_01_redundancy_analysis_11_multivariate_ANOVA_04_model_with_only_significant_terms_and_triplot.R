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
# RDA with significant term (factor ele)
# ------------------------------------------------------------------------------

ele.rda.out <- rda(spe.hel[1:27, ]~ ., as.data.frame(ele.fac))


summary(ele.rda.out)



# ------------------------------------------------------------------------------
# Triplot with "wa" sites related to factor centroids, and species arrows
# ------------------------------------------------------------------------------

plot(ele.rda.out, scaling = 1, display = "wa", main = "Multivariate ANOVA, factor elevation - scaling 1 - wa scores")

ordispider(ele.rda.out, ele.fac, scaling = 1, label = TRUE, col = "blue")

spe.sc1 <- scores(ele.rda.out, scaling = 1, display = "species")

arrows(0, 0, spe.sc1[, 1] * 0.3, spe.sc1[, 2] * 0.3, length = 0.1, angle = 10, col = "red")

text(spe.sc1[, 1] * 0.3, spe.sc1[, 2] * 0.3, labels = rownames(spe.sc1), pos = 4, cex = 0.8, col = "red")



# -->
# Cleanly shows the relationships of selected groups of species with elevation.
# This factor can be seen as a proxy for several important environmental variables, so that this analysis is quite informative.

# This figure also allows one to compare the dispersions of the levels on the ordination axes.


# Here, the 1st axis discriminates between the low-elevation sites one the left, and the mid- and high-elevation
# sites on the right.
# The dispersion of the "wa" scores of the low-elevation sites is much smaller on the first axis than the dispersion of the tow other groups.

# The 2nd axis contrasts the mid- and high elevation sites, with the low-elevation sites in-between.
# The within-group dispersions of the 3 groups of sites on this axis are about the same.

