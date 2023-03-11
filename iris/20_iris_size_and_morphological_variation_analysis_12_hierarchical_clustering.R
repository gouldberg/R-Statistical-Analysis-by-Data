setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iris
# ------------------------------------------------------------------------------
data("iris")


str(iris)

car::some(iris)



# ------------------------------------------------------------------------------
# Hierarchical Clustering
#   - If the initial aim is toe define clusters from the data without trying to understand the relationship between data,
#     the "ward" method will have the advantage of finding rather spherical clusters.
#     But if the idea is to appraise the structure of the data in the form space, the "average" method can be more easily understand.
# ------------------------------------------------------------------------------

rownames(iris) <- paste(toupper(substr(iris[,5], 1, 2)), rownames(iris), sep = "")

bb_ave <- hclust(dist(iris[,1:4]), method = "ave")
bb_ward <- hclust(dist(iris[,1:4]), method = "ward")

dend_ave <- as.dendrogram(bb_ave)
dend_ward <- as.dendrogram(bb_ward)



# ----------
par(mfrow=c(1,2))
plot(dend_ave)
plot(dend_ward)



# ----------
# For displaying different colors and symbols for tips of the dendrogram,
# we modify the class of the dendrogram object with a local function placed as argument of the dendrapply function
local({
  colLab <<- function(n){
    if(is.leaf(n)){
      a <- attributes(n)
      i <<- i + 1
      attr(n, "nodePar") <-
        c(a$nodePar, list(lab.col = mycols[i], pch = mysymbols[i], col = mycols[i], lab.cex = 0.5, cex = 0.5))
    }
    n
  }
  
  mycols <- c("blue", "red", "green")[as.factor(substr(labels(dend), 1, 2))]
  mysymbols <- c(15, 17, 1)[as.factor(substr(labels(dend), 1,2))]
  i <- 0
})


b <- dendrapply(dend, colLab)

plot(b, main = "UPGMA on the iris dat set")



# ------------------------------------------------------------------------------
# Hierarchical Clustering: representing on a circular tree
# ------------------------------------------------------------------------------

library(ade4)

kk <- hclust2phylog(bb, FALSE)

radial.phylog(kk, clabel.l = 0.5, cleaves = 0, circle = 1.7)

points(0, 0, pch = 21, cex = 2, bg = "grey")

