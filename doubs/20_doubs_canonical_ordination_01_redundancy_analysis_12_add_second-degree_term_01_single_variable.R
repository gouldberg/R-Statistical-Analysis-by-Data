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
# Nonlinear relationships in RDA
#  - RDA with a single second degree explanatory variable
#    Adding a second-degree term may indeed add exlanatory power to the model and improve its fits
# ------------------------------------------------------------------------------

# In Doubs data, there are several species that are found mostly in the central section of the river.
# Their relationship with the variable "distance from the source" (dfs) is therefore unimodal: absence first, then presence, then absence again.
# Such a simple case could be a good candidate to experiment with a 2nd-degree polynomial.



# Create a matrix of dfs and its orthogonal second degree term using function poly()

( dfs.df <- poly(dfs, 2) )

colnames(dfs.df) <- c("dfs", "dfs2")


# Verify that the polynomial terms are orthogonal
cor(dfs.df)



# ----------
# Find out if both variables are significant
adespatial::forward.sel(spe.hel, dfs.df)



# ----------
# RDA
spe.dfs.rda <- rda(spe.hel ~ ., as.data.frame(dfs.df))


summary(spe.dfs.rda)


RsquareAdj(spe.dfs.rda)



# ----------
# test
anova(spe.dfs.rda)



# ------------------------------------------------------------------------------
# Triplot using "lc" (model) site scores and scaling 2
# (interested primarily in the relationships among the species)
# ------------------------------------------------------------------------------

graphics.off();  par(mfrow = c(1,1));

triplot.rda(spe.dfs.rda, site.sc = "lc", scaling = 2, plot.sites = FALSE, pos.env = 1, mult.arrow = 0.9, move.origin = c(-0.25, 0), mar.percent = 0)



# -->
# Note that species having their optimum around mid-river will point to the opposite direction from the dfs-squared variable dfs2
# Species with arrows pointing in the same direction as the dfs2 variable may be more present at both ends of the river than in the middle.
# (no species shows this particular distribution)
# Arrows of species that are more present at one end of the river point in the direction of the dfs variable or opposite to it.




# ------------------------------------------------------------------------------
# Triplot using "wa" scores and scaling 2
# ------------------------------------------------------------------------------

# If you want a triplot showing the sites in a configuration closer to the data, replace "lc" by "wa"

triplot.rda(spe.dfs.rda, site.sc = "wa", scaling = 2, plot.sites = FALSE, pos.env = 1, mult.arrow = 0.9, move.origin = c(-0.25, 0), mar.percent = 0)



# ------------------------------------------------------------------------------
# Alternate code using plot.cca
# ------------------------------------------------------------------------------

plot(spe.dfs.rda, scaling = 2, display = c("sp", "lc", "cn"), main = "Triplot RDA spe ~ dfs+dfs2 - scaling 2 - lc scores")

spe6.sc <- scores(spe.dfs.rda, choices = 1:2, scaling = 2, display = "sp")

arrows(0, 0, spe6.sc[, 1] * 0.9, spe6.sc[, 2] * 0.9, length = 0, lty = 1, col = "red")



# ------------------------------------------------------------------------------
# Interpretation
# ------------------------------------------------------------------------------

# Maps of four fish species

par(mfrow = c(2, 2))

plot(spa, asp = 1, col = "brown", cex = spe$Satr, xlab = "x (km)", ylab = "y (km)", main = "Brown trout")
lines(spa, col = "light blue")

plot(spa, asp = 1, col = "brown", cex = spe$Thth, xlab = "x (km)", ylab = "y (km)", main = "Grayling")
lines(spa, col = "light blue")

plot(spa, asp = 1, col = "brown", cex = spe$Alal, xlab = "x (km)", ylab = "y (km)", main = "Bleak")
lines(spa, col = "light blue")

plot(spa, asp = 1, col = "brown", cex = spe$Titi, xlab = "x (km)", ylab = "y (km)", main = "Tench")
lines(spa, col = "light blue")



# -->
# The brown trout (Satr) is most strongly linked to the upper half of the river, its vector is opposed to dfs and orthogonal to dfs2.
# Grayling (Thth) is characteristic of the central part of the river, its vector on the triplot is opposed to that of dfs2.
# The bleak (Alal) is abundant in the lower half of the river
# Finally, the tench (Titi) is present in 3 different zones along the river, which results in vector pointing halfway between the dfs and dfs2 vectors.


