setwd("//media//kswada//MyFiles//R//factors_underlying_yield_in_two_fields")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Field 1 (Sand)
#  - Field 1 has the shape of a trapezoid about twice as long in the north-south direction as in the east-west direction
# ------------------------------------------------------------------------------
data.Set4.1 <- read.csv("//media//kswada//MyFiles//data//factors_underlying_yield_in_two_fields//Set4.196sample.csv", header = TRUE)

str(data.Set4.1)

xtabs(Sand ~ Row + Column, data = data.Set4.1)



# ----------
# The data for plotting with the function persp() must be in the form of a matrix
Sand <- matrix(nrow = 13, ncol = 7)
for (i in 1:nrow(data.Set4.1)) Sand[data.Set4.1$Row[i], data.Set4.1$Column[i]] <- data.Set4.1$Sand[i]

head(Sand)



# ------------------------------------------------------------------------------
# perspective plot
# ------------------------------------------------------------------------------
North <- 3 * 1:13
West <- 3 * 1:7
persp(North, West, Sand, theta = 30, phi = 20, zlim = c(0,45), scale = FALSE)


# --> 
# There is a strong north-south trend in sand content.
# Also, some interaction between x (east-west) and y (north-south) direction


# ------------------------------------------------------------------------------
# Trend plus random component for Field 4.1
#  - Linear model (quadratic)
# ------------------------------------------------------------------------------
trend.lm <- lm(Sand ~ Row + Column + I(Row^2) + I(Column^2) + I(Row*Column), data = data.Set4.1)

summary(trend.lm)


# -->
# Column^2 coefficient is very small
# This function captures the main feature of the trend: its rapid increas in the Row direction at the sourth end of the field
# Ths slight increase in the x (east-west) direction in the north end as well as the interaction may well be spurious, but in any case, they are not pronounced.

( b <- coef(trend.lm) )



# ----------
Trend <- matrix(nrow = 13, ncol = 7)

for (i in 1:8){
  for (j in 1:7){
    Trend[i,j] <- b[1] + b[2]*i + b[3]*j + b[4]*i^2 + b[5]*j^2 + b[6]*i*j
  }}

for (i in 9:13){
  for (j in 1:6){
    Trend[i,j] <- b[1] + b[2]*i + b[3]*j + b[4]*i^2 + b[5]*j^2 + b[6]*i*j
  }}


persp(North, West, Trend, theta = 30, phi = 20, zlim = c(0,45), scale = FALSE)



# ------------------------------------------------------------------------------
# Trend plus random component for Field 4.1
#  - Median polish for Field 4.1
# ------------------------------------------------------------------------------
# Break into 2 parts, one with 7 columns and one with 6
Sand1 <- Sand[1:8,]
Sand2 <- Sand[9:13,1:6]


trend1 <- medpolish(Sand1)
trend2 <- medpolish(Sand2)

trend1
trend2



# ----------
T1.mp <- matrix(nrow = 8, ncol = 7)

for (i in 1:8){
  for (j in 1:7){
    T1.mp[i,j] <- trend1$row[i] + trend1$col[j] + trend1$overall
  }}

T2.mp <- matrix(nrow = 5, ncol = 7)

for (i in 1:5){
  for (j in 1:6){
    T2.mp[i,j] <- trend2$row[i] + trend2$col[j] + trend2$overall
  }}

MedPolish <- rbind(T1.mp,T2.mp)

persp(North, West, MedPolish, theta = 30, phi = 20, zlim = c(0,45), scale = FALSE)


# -->
# The median polish approximation appears to fit the data bettern than the liinear model in the y (north-south) direction 
# and about the same in the x (east-west) direction (where there is little if any trend)



# ------------------------------------------------------------------------------
# Bubble plots of residuals of Field 4.1 sand content
# --> Detrended sand content based on linear model and on median polish, respectively
# ------------------------------------------------------------------------------
library(maptools)
library(sf)


# ----------
# read polygon data and convert to SpatialPolygonsDataFrame
# Set4.1.bdry.sf <- st_read("//media//kswada//MyFiles//references//SpatialDataAnalysisInEcologyAndAgricultureUsingR//Created//Set419697bdry.shp")
Set4.1.bdry.sf <- st_read("//media//kswada//MyFiles//R//factors_underlying_yield_in_two_fields//Created_kw//Set419697bdry_kw.shp")
Set4.1.bdry.sp <- as(Set4.1.bdry.sf, "Spatial")
Set4.1.bdry.sp



# ----------
coordinates(data.Set4.1) <- c("Easting", "Northing")



# ----------
i <- data.Set4.1$Row
j <- data.Set4.1$Column
data.Set4.1$lin <- data.Set4.1$Sand - (b[1] + b[2]*i + b[3]*j + b[4]*i^2 + b[5]*j^2 + b[6]*i*j)

# Concatenate the columns of T1.mp into a vector
stack(data.frame(T1.mp))[,1] # Chack that the concat. is correct

data.Set4.1$mp <- data.Set4.1$Sand - c(stack(data.frame(T1.mp))[,1], stack(data.frame(T2.mp))[1:30,1])



# ----------
par(mfrow=c(2,2))
par(mai = c(0.4, 0.4, 0.4, 0.4))

plot(Set4.1.bdry.sp, axes = TRUE)
plot(data.Set4.1, add = TRUE, pch = 1, cex =  (data.Set4.1$Sand / 15))
title(main = "Field 4.1 Sand Content", cex.main = 2, xlab = "Easting", ylab = "Northing", cex.lab = 1.5)
legend(592450, 4270600, c("15", "30", "45"), pt.cex = 1:3, pch = 1, y.intersp = 1.5, title = "Percent Sand")

plot(Set4.1.bdry.sp, axes = TRUE)
plot(data.Set4.1, add = TRUE, pch = 1, cex = (1 + data.Set4.1$lin / 10))
title(main = "Linear Detrending", cex.main = 2, xlab = "Easting", ylab = "Northing", cex.lab = 1.5)
legend(592450, 4270600, c("-10","0", "10"), pt.cex = c(0.1,1,2), pch = 1, y.intersp = 1, title = "Percent Sand")

plot(Set4.1.bdry.sp, axes = TRUE)
plot(data.Set4.1, add = TRUE, pch = 1, cex = (1 + data.Set4.1$mp / 10))
title(main = "Median Polish Detrending", cex.main = 2, xlab = "Easting", ylab = "Northing", cex.lab = 1.5)
legend(592450, 4270600, c("-10","0", "10"), pt.cex = c(0.1,1,2), pch = 1, y.intersp = 1, title = "Percent Sand")

