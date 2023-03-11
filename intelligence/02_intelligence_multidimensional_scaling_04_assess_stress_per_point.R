setwd("//media//kswada//MyFiles//R//intelligence")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  intelligence
# ------------------------------------------------------------------------------

data(intelligence, package = "smacof")


intelligence

car::some(intelligence)



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Bupple plot and stress plot
#   - computed by first averaging the squared representation errors related to p.
#     We then express this value as the percentage of the sum of all representation errors
# ------------------------------------------------------------------------------

fit$spp



# ----------
# Bubble plot
graphics.off()
par(mfrow=c(1,2))

plot(fit, plot.type = "bubbleplot")


# Stress Plot
plot(fit, plot.type = "stressplot")



# -->
# It show that there are no true outliers, but quite some scatter in terms of fit among the points.

# When assessing SPP values, one should first note that they become smaller the more points there are,
# because the SPP's are just contribution percentages of the total raw Stress.



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Distribution of the representation errors by Heatmap
# ------------------------------------------------------------------------------

library(gplots)

# Representation Errors
RepErr <- as.matrix((fit$dhat - fit$confdist) ^ 2)

yr <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)

heatmap.2( RepErr, cellnote = round(RepErr,2), Rowv = NA, Colv="Rowv", 
           lwid = c(1, 40), lhei = c(1, 40), margins = c(8, 8),
           key=FALSE, notecol="black", trace="none", col=yr, symm=TRUE, dendrogram="none" )     


