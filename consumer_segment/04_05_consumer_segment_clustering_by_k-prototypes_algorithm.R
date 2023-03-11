# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//consumer_segment")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Consumer Segment Data (created data)
# ------------------------------------------------------------------------------

seg.df <- read.csv("rintro-chapter5.csv", header = TRUE)


str(seg.df)


Hmisc::describe(seg.df)

summary(seg.df)

car::some(seg.df)


# remove the known segment assignments
seg.df2 <- seg.df[, -7]


# 300 * 6
dim(seg.df2)



# ---------------------------------------------------------------------------
# Assess number of groups
# ---------------------------------------------------------------------------

library(clustMixType)



Es <- numeric(15)

# nstart: if > 1 repetitive computations with random initializations are computed and the result with minimum tot.dist is returend.
for(i in 1:15){
  kpres <- kproto(seg.df2, k = i, nstart = 5)
  Es[i] <- kpres$tot.withinss
}

plot(1:15, Es, type = "b", ylab = "Objective Function", xlab = "# Clusters", main = "Scree Plot")




# ---------------------------------------------------------------------------
# k-prototypes clustering
# ---------------------------------------------------------------------------
k <- 4
kpres <- kproto(x = seg.df2, k = k, nstart = 5)



# ----------
kpres



# ---------------------------------------------------------------------------
# check the clusters variation
# ---------------------------------------------------------------------------

summary(kpres)



# ----------
graphics.off()
par(mfrow = c(1, 1))
clprofiles(kpres, seg.df2)




# ---------------------------------------------------------------------------
# Visualize the clusters
#   - clusplot() will perform dimensional reduction with principal components or multidimensional scaling as the data warrant,
#     and then plot the observations with cluster membership identified.
# ---------------------------------------------------------------------------

library(cluster)


# lines = 0: omit distance lines between groups
# labels = 3: plot data label
clusplot(seg.df2, kpres$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "k-prototype plot (k = 4)")
# clusplot(seg.df2, kpres$cluster, color = TRUE, shade = TRUE, labels = 3, lines = 0, main = "k-prototype plot (k = 4)")



