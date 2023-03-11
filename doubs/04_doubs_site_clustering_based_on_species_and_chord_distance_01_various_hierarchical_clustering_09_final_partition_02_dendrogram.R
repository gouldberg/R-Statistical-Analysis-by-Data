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
# Final Dendrogram
# ------------------------------------------------------------------------------

# Reorder clusters
spe.chwo <- reorder(spe.ch.ward, spe.ch)



# ----------
# plot reordered dendrogram with group labels
graphics.off();  par(mfrow=c(1,1));

plot(spe.chwo, hang = -1, xlab = "4 groups", sub = "", ylab = "Height", main = "Chord - Ward (reordered)", labels = cutree(spe.chwo, k = k))

rect.hclust(spe.chwo, k = k)




# ------------------------------------------------------------------------------
# plot the final dendrogram with group colors
# ------------------------------------------------------------------------------

source("./functions/hcoplot.R")


hcoplot(spe.ch.ward, spe.ch, lab = rownames(spe), k = 4)




# ------------------------------------------------------------------------------
# plot the final dendrogram with group colors (bar type)
# ------------------------------------------------------------------------------

dend <- as.dendrogram(spe.chwo)

dend %>% set("branches_k_color", k = k) %>% plot

clusters <- cutree(dend, k)[order.dendrogram(dend)]

dend %>% set("branches_k_color", k = k, value = unique(clusters) + 1) %>% plot

colored_bars(clusters + 1, y_shift = -0.5, rowLabels = paste(k, "clusters"))



