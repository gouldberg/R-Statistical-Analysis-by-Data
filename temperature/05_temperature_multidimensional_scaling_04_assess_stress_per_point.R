setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Bupple plot and stress plot
#   - computed by first averaging the squared representation errors related to p.
#     We then express this value as the percentage of the sum of all representation errors
# ------------------------------------------------------------------------------


fitv_o$spp

fiti_o$spp



# ----------
# Bubble plot
graphics.off()
par(mfrow=c(1,2))

plot(fitv_o, plot.type = "bubbleplot")

plot(fiti_o, plot.type = "bubbleplot")



# ----------
# Stress Plot
graphics.off()
par(mfrow=c(1,2))

plot(fitv_o, plot.type = "stressplot")

plot(fiti_o, plot.type = "stressplot")



# -->
# It show that there are some outliers

# When assessing SPP values, one should first note that they become smaller the more points there are,
# because the SPP's are just contribution percentages of the total raw Stress.



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Distribution of the representation errors by Heatmap
# ------------------------------------------------------------------------------

library(gplots)

# Representation Errors
RepErr <- as.matrix((fitv_o$dhat - fitv_o$confdist) ^ 2)

yr <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)

heatmap.2(RepErr, cellnote = round(RepErr,2), Rowv = NA, Colv="Rowv", 
           lwid = c(1, 40), lhei = c(1, 40), margins = c(8, 8),
           key=FALSE, notecol="black", trace="none", col=yr, symm=TRUE, dendrogram="none" )     


