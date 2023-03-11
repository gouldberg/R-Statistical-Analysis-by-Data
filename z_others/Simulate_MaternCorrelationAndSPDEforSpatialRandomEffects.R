# ------------------------------------------------------------------------------
# Matern Correlation and SPDE to calculate spatial random effects
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Generate sample spatial positions
# ------------------------------------------------------------------------------
sample_n <- 100

set.seed(123)
X1 <- runif(sample_n, min = 0, max = 100)
Y1 <- runif(sample_n, min = 0, max = 100)

par(mfrow=c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = X1, y =Y1, pch = 16, xlab = "X-coordinate", ylab = "Y-coordinate")


# histogram of distance between each site and its cumulative proportion 
( S <- cbind(X1, Y1) )
( D <- dist(S) )
summary(D)

par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
hist(D, freq = TRUE, main = "",  xlab = "Distance between sites", ylab = "Frequency")
text(5, 650, "A", cex = 1.5)
# plot(ecdf(D))

plot(x = sort(D), y = (1:length(D))/length(D), type = "l", xlab = "Distance between sites", ylab = "Cumulative proportion")
text(5, 1, "B", cex = 1.5)



# ------------------------------------------------------------------------------
# Simulating Calculate Matern correlation values versus Distance by varying kappa
# 
# For calculation of Matern correlation values, we set v = 1, which is also the default value in R-INLA 
# ------------------------------------------------------------------------------
# library(fields) for besselK
d.vec <- seq(0, max(D), length = 1000)      

AllCor <- NULL
Allr <- NULL
MyKappa <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5, 1, 2)

# In line with R-INLA, v = 1
# the range "r" in a variogram is defined as the distance at which the spatial dependency diminishes,  we set val = 8 and r = sqrt(val * v) / kappa
# r:  approximate distance at which the Cor.M becomes smaller than 0.1
v <- 1
val <- 8
for (kappa in MyKappa){
  Cor.M <- (kappa * d.vec) * besselK(kappa * d.vec, 1) 
  Cor.M[1] <- 1
  #lines(d.vec, Cor.M)
  r <- sqrt(var*v) / kappa
  #abline(v = r, lty = 2)
  AllCor <- c(AllCor, Cor.M)
  Allr <- c(Allr, r)
}     

MyData <- data.frame(
  AllCor = AllCor,
  AllDist = rep(d.vec, length(MyKappa)),
  AllID   = factor(rep(MyKappa, each = length(d.vec))))

MyData %>% head(10)
dim(MyData)

MyData$fAllID <- factor(paste("kappa = ", MyData$AllID, sep = ""))     
MyData2 <- subset(MyData, AllCor > 0.01)
MyData3 <- data.frame(Allr = Allr, fAllID = levels(MyData2$fAllID))    


# plot Cor.M (Matern Correlation Values) versus Distance between two sites
p <- ggplot()
p <- p + geom_line(data = MyData2, aes(y = AllCor, x = AllDist))
p <- p + xlab("Distance") + ylab("Correlation")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_vline(data = MyData3, aes(xintercept = Allr), linetype = "longdash")
p <- p + geom_hline(yintercept = 0.1, linetype = "longdash")                     
p <- p + facet_wrap( ~ fAllID, scales = "free_x")
p


# ------------------------------------------------------------------------------
# Make the mesh
# ------------------------------------------------------------------------------
library(INLA)

# Define a mesh for the sampling locations
loc <- S

# We need a grid on top of our sampling points
# max.edge: maximum allowed triangle edge lengths in the inner domain and in the outer extension
# cutoff: minimum allowed distance between points. Points at a closer distance than the supplied value are replaced by a single vertex
mesh1 <- inla.mesh.2d(loc, max.edge=c(10), cutoff = 0.5)

par(mfrow=c(1,1), mar=c(0,0,2,0))
plot(mesh1)
points(loc, col = 1, pch = 16, cex = 2)

mesh1$n


# ------------------------------------------------------------------------------
# Make the mesh for simulated data
# ------------------------------------------------------------------------------
sample_n <- 5

set.seed(123)
x1 <- runif(sample_n)
y1 <- runif(sample_n)

par(mfrow=c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = x1, y =y1, pch = 16, xlab = "X-coordinate", ylab = "Y-coordinate")


coords <- cbind(x1 = x1, y1 = y1)
mesh <- inla.mesh.2d(coords, max.edge=1)
plot(mesh)
points(coords, pch=16, cex = 2) 
# text(x =  mesh$loc[,1], y =  mesh$loc[,2], text = 1:31, col = 1, cex = 2) 
# text(x = coords[,1], y = coords[,2], 1:sample_n, cex = 3) 

# we have 31 vertices in the mesh
mesh$n
mesh$loc


# ------------------------------------------------------------------------------
# By finite element approach, error term u at site s(i) = sum( A * w )
# ------------------------------------------------------------------------------
A <- inla.spde.make.A(mesh, loc=coords)
str(A)
summary(A)
dim(A)

# Site 1 (the lower-left point) is vertex number 9 on the mesh
A


pl.dom <- cbind(c(0,1,1,0), c(0,0,1,1))
mesh <- inla.mesh.2d(pl.dom, max.edge=0.5)

par(mfrow=c(1,1))
plot(mesh)
points(coords, pch=16, cex = 2) 

text(x = coords[,1], y = coords[,2], 1:5, cex = 3) 
text(x =  mesh$loc[,1], y =  mesh$loc[,2], 1:41, col = 1, cex = 1.5)   


# ------------------------------------------------------------------------------
# The case where the sampling locations are inside a triangle:  not 0-1 values for "A"
#
# When making a mesh try to have triangles that have similar edge lengths and no sharp angles
# ------------------------------------------------------------------------------
pl.dom <- cbind(c(0,1,1,0), c(0,0,1,1))
mesh <- inla.mesh.2d(pl.dom, max.edge=0.5)

par(mfrow=c(1,1))
plot(mesh)
points(coords, pch=16, cex = 2) 

text(x = coords[,1], y = coords[,2], 1:5, cex = 3) 
text(x =  mesh$loc[,1], y =  mesh$loc[,2], 1:41, col = 1, cex = 1.5)   

A <- inla.spde.make.A(mesh,loc=coords)
str(A)
summary(A)
dim(A)
print(A, digits = 3)

mesh$n

table(rowSums(A>0))
table(rowSums(A))
table(colSums(A)>0)
