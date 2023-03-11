setwd("//media//kswada//MyFiles//R//wine")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wine
#   - 
# ------------------------------------------------------------------------------

wine <- read.table("wine.csv", header = TRUE, row.names = 1, sep = ";", check.names = FALSE)


# this produces error
str(wine)


# data("wine", package = "FactoMineR")

dim(wine)

head(wine)



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Bupple plot and stress plot
#   - computed by first averaging the squared representation errors related to p.
#     We then express this value as the percentage of the sum of all representation errors
# ------------------------------------------------------------------------------


fit_1i$spp

fit_2o$spp



# ----------
# Bubble plot
graphics.off()
par(mfrow=c(1,2))

plot(fit_1i, plot.type = "bubbleplot")

plot(fit_2o, plot.type = "bubbleplot")



# ----------
# Stress Plot
graphics.off()
par(mfrow=c(1,2))

plot(fit_1i, plot.type = "stressplot")

plot(fit_2o, plot.type = "stressplot")



# -->
# When assessing SPP values, one should first note that they become smaller the more points there are,
# because the SPP's are just contribution percentages of the total raw Stress.

# Note that the proportion curve is quite different from distmat + type = "interval" vs. distmat2 + type = "ordinal".
# Japan has quite large stress proportion in the latter case.



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Distribution of the representation errors by Heatmap
# ------------------------------------------------------------------------------

library(gplots)

# Representation Errors
RepErr <- as.matrix((fit_2o$dhat - fit_2o$confdist) ^ 2)

yr <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)

heatmap.2( RepErr, cellnote = round(RepErr,2), Rowv = NA, Colv="Rowv", 
           lwid = c(1, 40), lhei = c(1, 40), margins = c(8, 8),
           key=FALSE, notecol="black", trace="none", col=yr, symm=TRUE, dendrogram="none" )     


# -->
# Due to the large number of companies, this analysis shoud not be applied ...

