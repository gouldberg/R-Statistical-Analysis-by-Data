setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Bupple plot and stress plot
#   - computed by first averaging the squared representation errors related to p.
#     We then express this value as the percentage of the sum of all representation errors
# ------------------------------------------------------------------------------

result$spp



# ----------
# Bubble plot
graphics.off()
par(mfrow=c(1,2))

plot(result, plot.type = "bubbleplot")


# Stress Plot
plot(result, plot.type = "stressplot")



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Distribution of the representation errors by Heatmap
# ------------------------------------------------------------------------------

library(gplots)


# Representation Errors
RepErr <- as.matrix((res$dhat - res$confdist) ^ 2)

yr <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)

heatmap.2( RepErr, cellnote = round(RepErr,2), Rowv = NA, Colv="Rowv", 
           lwid = c(1, 40), lhei = c(1, 40), margins = c(8, 8),
           key=FALSE, notecol="black", trace="none", col=yr, symm=TRUE, dendrogram="none" )     


# ----------
# Representation Errors
RepErr <- as.matrix((result$dhat - result$confdist) ^ 2)

yr <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)

heatmap.2( RepErr, cellnote = round(RepErr,2), Rowv = NA, Colv="Rowv", 
           lwid = c(1, 40), lhei = c(1, 40), margins = c(8, 8),
           key=FALSE, notecol="black", trace="none", col=yr, symm=TRUE, dendrogram="none" )     

