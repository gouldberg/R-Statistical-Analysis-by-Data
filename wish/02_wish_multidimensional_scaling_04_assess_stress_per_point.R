setwd("//media//kswada//MyFiles//R//wish")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wish
# ------------------------------------------------------------------------------

data(wish, package = "smacof")


wish


car::some(wish)



# ------------------------------------------------------------------------------
# Asess Stress Per Point (SPP):  Bupple plot and stress plot
#   - computed by first averaging the squared representation errors related to p.
#     We then express this value as the percentage of the sum of all representation errors
# ------------------------------------------------------------------------------


fit$spp

res$spp



# ----------
# Bubble plot
graphics.off()
par(mfrow=c(1,2))

plot(fit, plot.type = "bubbleplot")

plot(res, plot.type = "bubbleplot")



# ----------
# Stress Plot
graphics.off()
par(mfrow=c(1,2))

plot(fit, plot.type = "stressplot")

plot(res, plot.type = "stressplot")



# -->
# It show that there are no true outliers, but quite some scatter in terms of fit among the points.
# Why France is the highest Stress contributor and Congo the country with the relatively best fit, needs to be investigated by further research.

# One reason coud be that France is a country that generates a relatively complex mental representation 
# so that different respondents may use different criteria when comparing it to other countries.
# Congo, on the other hand, could be just the opposite.
# The respondents may all perceive it in the same way as a developing African country.

# When assessing SPP values, one should first note that they become smaller the more points there are,
# because the SPP's are just contribution percentages of the total raw Stress.



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


# -->
# For France, note that the large SPP value is caused primarily by one single comparison,
# The perceived similarity of France and India.
# It seems that the respondents had problems to compare these two countries ("Similar? in what sense ?")
