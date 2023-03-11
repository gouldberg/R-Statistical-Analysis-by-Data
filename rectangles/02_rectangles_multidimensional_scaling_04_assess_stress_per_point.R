setwd("//media//kswada//MyFiles//R//rectangles")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rectangles
# ------------------------------------------------------------------------------

data(rectangles, package = "smacof")


str(rectangles)


car::some(rectangles)



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Bupple plot and stress plot
#   - computed by first averaging the squared representation errors related to p.
#     We then express this value as the percentage of the sum of all representation errors
# ------------------------------------------------------------------------------

res_mx$spp



# ----------
# Bubble plot
graphics.off()
par(mfrow=c(2,2))

plot(res_mx, plot.type = "bubbleplot")
plot(res_mh, plot.type = "bubbleplot")


# Stress Plot
plot(res_mx, plot.type = "stressplot")
plot(res_mh, plot.type = "stressplot")



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Distribution of the representation errors by Heatmap
# ------------------------------------------------------------------------------

library(gplots)


# Representation Errors
RepErr <- as.matrix((res_mx$dhat - res_mx$confdist) ^ 2)

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

