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
# Two PCA biplots by biplot.rda: scaling 1 and scaling 2
#    - Following the tradition, OBJECTS are represented as points, and variables are displayed as arrows
#    - Scaling 1: optimal display of distance relationships among objects
#    - Scaling 2: optimal display of covariances among variables
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

biplot(env.pca, scaling = 1, main = "PCA - scaling 1")

biplot(env.pca, main = "PCA - scaling 2")  # Default scaling 2



# ------------------------------------------------------------------------------
# Two PCA biplots by cleanplot.pca
#
# A rectangular graphic window is needed to draw the plots together
# scaling 1 biplot
#  - the circle is called a circle of equilibrium contribution.
#    Its radius is equal to square root of d/p (= 2/11, d: numbers of axes in the biplot, p: dimensions of the PCA space)
#    The radius of this circle represents the length of the vector representing a variable that would contribute equally to all dimensions of the PCA space.
#    The variables that have vectors longer than this radius make a higher contribution than average and can be interpreted with confidence.
# scaling 2 biplot
#  - projection into a Mahalanobis space. The variables are organized in groups.
# ------------------------------------------------------------------------------

source("./functions/cleanplot.pca.R")


graphics.off()

par(mfrow = c(1, 2))

cleanplot.pca(env.pca, scaling = 1, mar.percent = 0.08)

cleanplot.pca(env.pca, scaling = 2, mar.percent = 0.04)



# ----------
# just compare
graphics.off();  par(mfrow=c(2,2));

biplot(env.pca, scaling = 1, main = "PCA - scaling 1")

biplot(env.pca, main = "PCA - scaling 2")

cleanplot.pca(env.pca, scaling = 1, mar.percent = 0.08)

cleanplot.pca(env.pca, scaling = 2, mar.percent = 0.04)




# -->
# Scaling 1 biplot: Note that ARCH type shape is shown.
#  - a gradient from left to right, starting with a group formed by sites 1-10 which display the highest values of elevation (ele) and slope (slo),
#    and the lowest values in river discharge (dis), distance from the source (dfs) and hardness (har).
#  - The second group of sites (11-16) has the highest values in oxygen content (oxy) and the lowest in nitrate concentration (nit).
#  - A third group of very similar sites (17-22) show intermediate values in almost all the measured variables; they are not spread out by the variables contributing to axes 1 and 2.
#  - Phosphate (pho) and ammonium (amm) concentrations, as well as biological oxygen demand (bod) show their maximum values around sites 23-25; the values decrease afterwards.
#  - Overall, the progression from oligotrophic, oxygen-rich to eutrophic, oxygen-deprived water is clear.

# Scaling 2 biplot shows the variables are organized in groups. (FAR MORE INFORMATIVE THAN THE VISUAL EXAMINATION OF A CORRELATION MATRIX)
#  - The lower left part of the biplot shows that evelevation and slope are very highly, positively correlated,
#    and that these 2 variables are very highly, negatively correlated with another group comprising distance from the source, river discharge and hardness.
#  - Oxygen content is positively correlatted with slope and elevation, but very negatively with phosphate and ammonium concentration and,
#    of course, with biological oxygen demand.
#  - The right part of the diagram shows the variables associated with the lower section of the river, i.e., the group discharge and hardness,
#    which are highly correlated with the distance from the source, and the group of variables linked to eutrophication, i.e. phosphate, ammonium concentration
#    and biological oxygen demand.
#  - Positively correlated with these 2 groups is nitrate concentration.
#  - Nitrate and pH have nearly orthogonal arrows, indicating a correlation close to 0.
#    pH displays a shorter arrow, showing its lesser importance for the ordination of the sites in the ordination plane.
#    A plot of axes 1 and 3 would emphasize its contribution to axis 3.



# ------------------------------------------------------------------------------
# biplot by other axes
# ------------------------------------------------------------------------------
# biplot by axes 1,2 and 1,3, and 2,3
# pH displays a shorter arrow in space of axes 1 and 2, but longer in space by axes 1 and 3

par(mfrow = c(2, 2))

biplot(env.pca, main = "PCA - scaling 2", choices = c(1,2))

biplot(env.pca, main = "PCA - scaling 2", choices = c(1,3))

biplot(env.pca, main = "PCA - scaling 2", choices = c(2,3))



# ------------------------------------------------------------------------------
# Plots with a subset of variables: ele, oxy, har, bod, 
# using cleanplot.pca
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

var.subset <- c(2, 6, 10, 11)

cleanplot.pca(env.pca, scaling = 1, select.spe = var.subset, mar.percent = 0.10)

cleanplot.pca(env.pca, scaling = 2, select.spe = var.subset, mar.percent = 0.04)



# ------------------------------------------------------------------------------
# Plots with a subset of variables: ele, oxy, har, bod, 
# using biplot {vegan}
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))


# Scaling 1
var.sc1.sub <- scores(env.pca,  scaling = 1, display = "species")[c(2, 6, 10, 11), ]

biplot(env.pca, scaling = 1, main = "PCA scaling 1", type = "n")

text(env.pca, scaling = 1, display = "sites", cex = 0.7)

arrows(0, 0, var.sc1.sub[, 1], var.sc1.sub[, 2], length = 0.10, angle = 10, col = "red")

text(var.sc1.sub[, 1], var.sc1.sub[, 2], labels = rownames(var.sc1.sub), col = "red", pos = 4)



# ----------
# Scaling 2
var.sc2.sub <- scores(env.pca, display = "species")[c(2, 6, 10, 11), ]

biplot(env.pca, type = "n", main = "PCA scaling 2")

text(env.pca, scaling = 2, display = "sites", cex = 0.7)

arrows(0, 0, var.sc2.sub[, 1], var.sc2.sub[, 2], length = 0.10, angle = 10, col = "red")

text(var.sc2.sub[, 1], var.sc2.sub[, 2], labels = rownames(var.sc2.sub), col = "red", pos = 4)



