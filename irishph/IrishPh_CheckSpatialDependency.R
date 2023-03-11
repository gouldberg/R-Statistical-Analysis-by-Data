# ------------------------------------------------------------------------------
# Data: IrishPh
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "sp", "gstat", "ggmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file = "./ZuurSpatialTemporalAndSpatialTemporalEcologicalDataAnalysiswithRINLA/IrishPh.txt", header=T, dec=".")
dim(data)
str(data)

Hmisc::describe(data)

is.na(data)
colSums(is.na(data))


data$fForested <- factor(data$Forested, levels = c(1, 2), labels = c("Yes", "No"))


# ------------------------------------------------------------------------------
# Simple linear regression by each covariate (Altitude and SDI)
# ------------------------------------------------------------------------------
MyData <- data.frame(
  Y        = rep(data$pH, 2),
  Cov      = c(data$SDI, data$Altitude),
  Forested = rep(data$fForested, 2),
  ID = rep(c("SDI", "Altitude"), each = nrow(data)))

p <- ggplot()
p <- p + geom_point(data = MyData, aes(y = Y, x = Cov), shape = 1,size = 1)
p <- p + xlab("Covariate") + ylab("pH")
p <- p + theme(text = element_text(size = 15))
p <- p + geom_smooth(data = MyData, aes(x = Cov, y = Y, group = Forested), method = "lm")
p <- p + facet_grid(.~ID, scales = "free_x")
p

# Alternative coding
theme_set(theme_gray() + theme(text = element_text(size = 15)))
update_geom_defaults("point", list(shape = 16, size = 2))
update_geom_defaults("line", list(lwd = 3))

ggplot(MyData, aes(x = Cov, y = Y, group = Forested, col = Forested)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ ID, scales = "free_x") + xlab("Covariate") + ylab("pH")


# ------------------------------------------------------------------------------
# Linear regression by SDI covariate and validate the model
# ------------------------------------------------------------------------------
M1 <- lm(pH ~ SDI, data = data)

# Std.Error for SDI coef is 0.001616 --> too small ....
summary(M1)

set.seed(12345)
par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = data$SDI, y = data$pH, xlab = "SDI", ylab = "pH")
abline(M1, lwd = 5)
text(70, 8.7, 'A', cex = 1.5)


# Simulate the data following the model M1
plot(x = data$SDI, y = data$pH, xlab = "SDI", ylab = "pH", type = "n")
abline(M1, lwd = 5)
text(70, 8.7, 'B', cex = 1.5)

MyDatai <- data.frame(SDI = seq(from = quantile(data$SDI, 0.15),
                                to   = 0.99 * max(data$SDI),
                                length = 10))

K <- length(MyDatai$SDI)
X <- matrix(nrow = 30, ncol = K)
Z <- matrix(nrow = 30, ncol = K)

for (i in 1:K){
  Xi   <- model.matrix(~ SDI, data = MyDatai)[i,]
  muNorm.i <- Xi %*% coef(M1)
  zi       <- rnorm(30, mean = muNorm.i, sd = 0.3805)
  points(x = rep(Xi[2], 30), y = zi, col = grey(0.5), pch = 16)    
  X[,i] <- Xi
  Z[,i] <- zi   
}

# two different sites. At both sites the pH value is below the fitted line.  The two sites are far away from each other OR close to each other ?
points(x = X[10,5], y = Z[10,5],  col = grey(0.1),  pch = 16, cex = 2)    
points(x = X[10,6], y = Z[10,6],  col = grey(0.1),  pch = 16, cex = 2)    


# ------------------------------------------------------------------------------
# Including up to three-way interactions
# ------------------------------------------------------------------------------
# Transform Altitude for including interations
data$LOGAltitude <- log10(data$Altitude)

M1 <- lm(pH ~ LOGAltitude + SDI + fForested + LOGAltitude : SDI  + LOGAltitude : fForested + SDI : fForested + LOGAltitude : SDI : fForested, data = data)
summary(M1)


# Classical model selection using the AIC
step(M1)


# Optimal model as judged by AIC --> two-way interactions with log(Altitude) and fForested
M3 <- lm(pH ~ LOGAltitude + SDI + fForested + LOGAltitude:fForested, data = data)
summary(M3)


# ------------------------------------------------------------------------------
# Model Validation  -->  does not detect any problems
# ------------------------------------------------------------------------------
# Standardized residuals versus fitted value
E3 <- rstandard(M3)
F3 <- fitted(M3)
par(mfrow=c(1,1))
plot(x = F3, y = E3)
abline(h = 0, v = 0)

# Normality
hist(E3)

# Independence due to model misfit
plot(x = data$LOGAltitude, y = E3)
abline(h = 0, v = 0)

plot(x = data$SDI, y = E3)
abline(h = 0, v = 0)

boxplot(E3 ~ fForested, data = data)
abline(h = 0)


# ------------------------------------------------------------------------------
# Model Validation:  Spatial patterns in the residuals?
# ------------------------------------------------------------------------------
# It seems that the majority of the negative residuals are along the southeast caost.  This may indicate violation of spatial independence
MyCex <- 3 * abs(E3) / max(E3) + 0.5
Sign <- as.numeric(E3 >= 0) + 1
MyPch <- c(1, 16)[Sign]
xyplot(Northing ~ Easting, data = data, cex = MyCex, pch = MyPch, col = 1, aspect = "iso",
       xlab = list(label = "Easting", cex = 1.5),
       ylab = list(label = "Northing", cex = 1.5)
)


# Check the variogram by each directions
# label 0:  the combinations of sites in north-south directions,  label 90 is for east-west directions ...
# It seems that we have clear dependency in east-west directions.
library(sp)
data$Easting.km  <- data$Easting/1000
data$Northing.km <- data$Northing/1000
mydata <- data.frame(E3, data$Easting.km, data$Northing.km)
coordinates(mydata) <- c("data.Easting.km", "data.Northing.km")
V1 <- variogram(E3 ~ data.Easting.km + data.Northing.km , data = mydata, cressie = TRUE, alpha=c(0, 45, 90, 135), cutoff=150)
plot(V1, xlab = list(label = "Distance", cex = 1.5), ylab = list(label = "Cressie's semivariance", cex = 1.5), col = 1, pch = 16, smooth = TRUE)


# Combinations of two sites for which the distance between the sites is within a certain interval.
# It seems that in east-west directions, the sites are more close to each other.
data$Xkm <- data$Easting / 1000
data$Ykm <- data$Northing / 1000

D <- matrix(nrow = 210, ncol = 210)
for (i in 1:210){
  for (j in 1:210){
    D[i,j] <- sqrt(  (data$Xkm[i]-data$Xkm[j])^2 + (data$Ykm[i]-data$Ykm[j])^2  )
  }
}
dim(D)

par(mfrow = c(2,2), cex.main = 1.5, type = "s")
Small <- D < 10
plot(x = data$Xkm, y = data$Ykm, type = "n", xlab = "Easting", ylab = "Northing", cex.lab = 1.5, main = "Distances < 10 km")

for (i in 1:210){
  for (j in 1:210){
    if (Small[i,j] & D[i,j] > 0){
      a1 <- c(data$Xkm[i], data$Xkm[j])
      a2 <- c(data$Ykm[i], data$Ykm[j])
      lines(a1, a2, lwd = 1)			
    }
  }
}     


Small <- D >= 10 & D < 20
plot(x = data$Xkm, y = data$Ykm, type = "n", xlab = "Easting", ylab = "Northing", cex.lab = 1.5, main = "10 km < Distances < 20 km ")

for (i in 1:210){
  for (j in 1:210){
    if (Small[i,j] & D[i,j] > 0){
      a1 <- c(data$Xkm[i], data$Xkm[j])
      a2 <- c(data$Ykm[i], data$Ykm[j])
      lines(a1, a2, lwd = 1)			
    }
  }
}     


Small <- D > 20 & D < 30
plot(x = data$Xkm, y = data$Ykm, type = "n", xlab = "Easting", ylab = "Northing", cex.lab = 1.5, main = "20 km < Distances < 30 km")

for (i in 1:210){
  for (j in 1:210){
    if (Small[i,j] & D[i,j] > 0){
      a1 <- c(data$Xkm[i], data$Xkm[j])
      a2 <- c(data$Ykm[i], data$Ykm[j])
      lines(a1, a2, lwd = 1)			
    }
  }
}     


Small <- D > 30 & D < 40
plot(x = data$Xkm, y = data$Ykm, type = "n", xlab = "Easting", ylab = "Northing", cex.lab = 1.5, main = "30 km < Distances < 40 km")

for (i in 1:210){
  for (j in 1:210){
    if (Small[i,j] & D[i,j] > 0){
      a1 <- c(data$Xkm[i], data$Xkm[j])
      a2 <- c(data$Ykm[i], data$Ykm[j])
      lines(a1, a2, lwd = 1)			
    }
  }
}     


# ------------------------------------------------------------------------------
# Simulate Variogram
# ------------------------------------------------------------------------------
library(gstat)
set.seed(1)
xy <- expand.grid(1:100, 1:100)
names(xy) <- c("x","y")

g.dummy <- gstat(formula=z~1,  locations=~x+y,  dummy=T,  beta=1,  model=vgm(psill=1,model="Exp",range=5),  nmax=20)
yy <- predict(g.dummy, newdata=xy, nsim=1) 

g2.dummy <- gstat(formula=z~1, locations=~x+y,  dummy=T,  beta=1,  model=vgm(psill=1,model="Exp",range=0.0001), nmax=20)
yy2 <- predict(g2.dummy, newdata=xy, nsim=1)

library(sp)

mydata <- data.frame(yy, xy$x, xy$y)
coordinates(mydata) <- c("xy.x", "xy.y")
V1 <- variogram(sim1 ~ xy.x + xy.y , data = mydata, cressie = TRUE)

par(mfrow=c(1,2))
plot(V1, xlab = list(label = "Distance", cex = 1.5), ylab = list(label = "Cressie's semivariance", cex = 1.5), col = 1, pch = 16, smooth = TRUE, cex = 1.5)

mydata <- data.frame(yy2, xy$x, xy$y)
coordinates(mydata) <- c("xy.x", "xy.y")
V1 <- variogram(sim1 ~ xy.x + xy.y , data = mydata, cressie = TRUE)
plot(V1, xlab = list(label = "Distance", cex = 1.5), ylab = list(label = "Cressie's semivariance", cex = 1.5), col = 1, pch = 16, smooth = TRUE, cex = 1.5)
