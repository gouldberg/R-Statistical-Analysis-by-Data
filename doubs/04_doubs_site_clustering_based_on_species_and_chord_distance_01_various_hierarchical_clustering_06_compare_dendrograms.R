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
# Compare 2 dendrograms to highlight common subtrees:  can you recognize particularly "robust" clusters ?
# ------------------------------------------------------------------------------

graphics.off();  par(mfrow=c(2,1));

plot(spe.ch.complete, labels = rownames(spe), main = "Site clustering \n based on species chord distance (complete linkage)")

plot(spe.ch.ward, labels = rownames(spe), main = "Site clustering \n based on species chord distance (ward.D2)")


dend.ward <- as.dendrogram(spe.ch.ward)

dend.comp <- as.dendrogram(spe.ch.complete)

dend <- dendextend::dendlist(dend.ward, dend.comp)


# Sites are ordered to match the two dendrogramss as well as possible
# Colours highlight common clusters, whereas sites printed in black have different positions in the 2 trees

dendextend::tanglegram(untangle(dend), sort=TRUE, common_subtrees_color_branches = TRUE, main_left = "Ward method", main_right = "Complete linkage")

