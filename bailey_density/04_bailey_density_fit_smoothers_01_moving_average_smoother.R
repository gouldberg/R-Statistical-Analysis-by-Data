# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ------------------------------------------------------------------------------
# Moving average smoothers:  manually computed
# ------------------------------------------------------------------------------

MyPch <- vector(length = length(DF$MeanDepth))

MyPch[DF$MeanDepth < 2000] <- 1

MyPch[DF$MeanDepth > 3000] <- 1

MyPch[DF$MeanDepth >= 2000 & DF$MeanDepth <= 3000] <- 16



# ----------
x1 <- seq(0, 0.03, length = 50)

y1 <- rep(2000, length(x1))



par(mfrow = c(1,2), mar = c(5.5,5,2,2))

plot(x = DF$MeanDepth, y = DF$Dens, xlab = MyXLab, ylab = MyYLab, pch = MyPch, cex.lab = 1.5)

points(x = 2500, y = -0.001, pch = 17, cex = 2)

lines(x = y1, y = x1, lwd = 3, lty = 2)

lines(x = y1 + 1000, y = x1, lwd = 3, lty = 2)



# ----------
tapply(DF$Dens, MyPch, mean)


# Moving average
MyDepth <- seq(from = 800, to = 4850, length = 150)

MyMean  <- vector(length = 150)

k <- 1

for (i in MyDepth){
  MyPch <-vector(length = length(DF$MeanDepth))
  l1 <- i - 500
  l2 <- i + 500
  MyPch[DF$MeanDepth < l1] <- 1
  MyPch[DF$MeanDepth > l2] <- 1
  MyPch[DF$MeanDepth >= l1 & DF$MeanDepth <= l2] <- 2
  m <- tapply(DF$Dens, MyPch, mean)
  MyMean[k] <- m[2]
  k <- k + 1
}



# ----------
par(mar = c(5,5,2,2))

plot(x = DF$MeanDepth, y = DF$Dens, xlab = MyXLab, ylab = MyYLab, pch = MyPch, cex.lab = 1.5)

lines(x = MyDepth, y = MyMean, lwd = 5)



# -->
# The moving average smoother results in a jaggled line


