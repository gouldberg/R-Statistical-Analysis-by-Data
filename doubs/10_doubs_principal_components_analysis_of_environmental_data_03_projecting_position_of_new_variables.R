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




# -----------------------------------------------------
# Projecting position of new variables in a PCA by predict()
# -----------------------------------------------------

# PCA of the environmental variables minus oxy and bod
# Create data frame with oxy and bod (our "new" variables)

env.pca2 <- rda(env[, -c(10, 11)], scale = TRUE)

new.var <- env[, c(10, 11)]



# ----------
# Compute position of new variables (oxy and bod) (arrow tips)
new.vscores <- predict(env.pca2, type = "sp", newdata = new.var, scaling = 2)



# ----------
# Plot of the result - scaling 2
biplot(env.pca2, scaling = 2)

arrows(0, 0, new.vscores[, 1], new.vscores[, 2], length = 0.05, angle = 30, col = "blue")

text(new.vscores[, 1], new.vscores[, 2], labels = rownames(new.vscores), cex = 0.8, col = "blue", pos = 2)



# ----------
# Projecting supplementary objects into a PCA biplot
# PCA and projection of supplementary sites 2, 9 and 23 using prcomp() {stats} and predict()
# Line numbers 8 and 22 are offset because of the deletion of empty site #8
# PCA using prcomp()
# Argument 'scale.= TRUE' calls for a PCA on a correlation matrix; it is equivalent to 'scale = TRUE' in function rda()

env.prcomp <- prcomp(env[-c(2, 8, 22), ], scale. = TRUE)



# Plot of PCA results using biplot.prcomp().
# Functions text() and points() do not seem to work with this biplot(env.prcomp, scale = 0)



# ----------
# Plot of PCA site scores using generic function plot()
plot(env.prcomp$x[ ,1], env.prcomp$x[ ,2], type = "n", main = "PCA scaling 1 - sites with supplementary objects", xlab = "PCA 1", ylab = "PCA 2")

abline(h = 0, col = "gray")

abline(v = 0, col = "gray")

text(env.prcomp$x[ ,1], env.prcomp$x[ ,2], labels = rownames(env[-c(2, 8, 22), ]))



# ----------
# Projection of new site scores into the PCA plot

new.sit <- env[c(2, 8, 22), ]

pca.newsit <- predict(env.prcomp, new.sit) 

text(pca.newsit[, 1], pca.newsit[, 2], labels = rownames(pca.newsit), cex = 0.8, col = "blue")
