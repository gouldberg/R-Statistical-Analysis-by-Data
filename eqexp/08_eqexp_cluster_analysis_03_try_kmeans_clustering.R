setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
#   - Various bivariate earthquakes (EQ) and explosions (EX) recorded at 40 pts/s including an even NZ (Novaya Zemlya)
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)



# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Cluster Analysis for Earthquakes and Explosions
# For comparison K-means clustering
# ------------------------------------------------------------------------------

library(cluster)


k <- 4

set.seed(201808)

eq.kmeans <- kmeans(JD, centers = k, nstart = 100)

eq.kmeans


par(mgp = c(1.6,.6,0), cex=3/4, cex.lab=4/3, cex.main=4/3)

clusplot(JD, eq.kmeans$cluster, col.clus=1, labels=3, lines=0, col.p=1, 
         main="Clustering Results for Explosions and Earthquakes")


