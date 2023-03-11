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



# ----------
M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)



# ------------------------------------------------------------------------------
# Visualization:  Y vs. X
# ------------------------------------------------------------------------------

# Observed Total Abundance values plotted versus Mean Depth
par(mfrow=c(1,1), mar = c(5,5,2,2))

MyData <- data.frame(MeanDepth = seq(804, 4865, length = 25))

P1 <- predict(M1, newdata = MyData, type = "response")

plot(x = DF$MeanDepth, y = DF$TotAbund, ylim = c(0,1300), xlab = "Mean depth (km)", ylab = "Total abundance values", cex.lab = 1.5)

lines(MyData$MeanDepth, P1, lwd = 3)



# ------------------------------------------------------------------------------
# Visualization:  Simulated Poisson distribution
# ------------------------------------------------------------------------------
# also the gray dots are 150 simulated values from a Poisson distribution with the mean given by the line.

range(DF$MeanDepth)

HL <- seq(804, 4865, length = 25)

Beta <- coef(M1)

for (i in 1:25){
  mu <- exp(Beta[1] + Beta[2] * HL[i])
  yi <- rpois(50, lambda= mu)
  points(jitter(rep(HL[i], 50)), jitter(yi), col = grey(0.5),  pch = 16, cex = 1)
}

lines(MyData$MeanDepth, P1, lwd = 3)



# ------------------------------------------------------------------------------
# Visualization:  in 3D
# ------------------------------------------------------------------------------

# 3-D scatterplot
library(scatterplot3d)

x <- seq(804, 4865, length = 25)
y <- exp(coef(M1)[1] + coef(M1)[2]*x)
y
z <- 0 * x

ymeas = rpois(length(y),lambda=y)
# plot(x,ymeas,type="p",xlab="Covariate",ylab="Observed values")
# lines(x,y)

rr = scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="black",
                   col.grid="black", pch=20, zlim=c(0, 0.05), type="l", lwd=3,
                   # ylim = c(9,1200),
                   cex.lab = 1.5, xlab="Mean depth (km)", ylab="Possible values",zlab="Probability")


MyX = c(1000, 2000, 3000, 4000, 5000)

for (i in 1:5){
  xi = MyX[i]
  yi = exp(coef(M1)[1] + coef(M1)[2] * xi)
  yseq = round(seq(0,500,by=10))
  zi = dpois(yseq, lambda=yi)
  rb = cbind(xi,yseq,zi)
  rr$points3d(rb, col = 1, type="h", pch=30)
  rdat <- cbind(DF$MeanDepth,DF$TotAbund, rep(0,nrow(DF)))
  #rr$points3d(rdat, col = 1,type="p")
}

