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
# Projection of supplementary sites in a CA
# ------------------------------------------------------------------------------

# scaling 1
# Data set with 3 sites removed
sit.small <- spe[-c(7, 13, 22), ]

sitsmall.ca <- vegan::cca(sit.small)

graphics.off();  par(mfrow=c(1,1));

plot(sitsmall.ca, display = "sites", scaling = 1)


# Passive projection of these removed 3 sites
newsit3 <- spe[c(7, 13, 22), ]

ca.newsit <- predict(sitsmall.ca, newsit3, type = "wa", scaling = 1)

text(ca.newsit[, 1], ca.newsit[, 2], labels = rownames(ca.newsit), cex = 0.8, col = "blue")




# ----------
# scaling 2
# Data set with 3 species removed

spe.small <- spe[, -c(1, 3, 10)]

spesmall.ca <- vegan::cca(spe.small)

plot(spesmall.ca, display = "species", scaling = 2)


# Passive projection of these removed 3 sites
newspe3 <- spe[, c(1, 3, 10)]

ca.newspe <- predict(spesmall.ca, newspe3, type = "sp",  scaling = 2)

text(ca.newspe[, 1], ca.newspe[, 2], labels = rownames(ca.newspe), cex = 0.8, col = "blue")



