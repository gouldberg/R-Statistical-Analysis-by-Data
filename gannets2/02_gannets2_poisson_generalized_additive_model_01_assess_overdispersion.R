setwd("//media//kswada//MyFiles//R//gannets2")

packages <- c("dplyr", "lattice", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gannets2
# ------------------------------------------------------------------------------

Gannets <- read.table(file = "Gannets2.txt", header = TRUE)


str(Gannets)
names(Gannets)



# ----------
library(lattice) #For multi-panel graphs
library(gamlss)  #For GAMs
library(mgcv)    #For GAMs
library(gstat)   # For checking spatial patterns



Gannets$Xkm <- Gannets$X / 1000
Gannets$Ykm <- Gannets$Y / 1000
Gannets2 <- Gannets[Gannets$Area_surveyedkm2 > 0,]
Gannets2$LArea <- log(Gannets2$Area_surveyedkm2)
Gannets2$TimeH <- Gannets2$Hours + Gannets2$Minutes/60
Gannets2$Date <- paste(Gannets2$Day,Gannets2$Month,Gannets2$Year, sep = "/")
Gannets2$DayInYear <- strptime(Gannets2$Date, "%d/%m/%Y")$yday + 1
Gannets2$DaySince0 <- ceiling(julian(strptime(Gannets2$Date, format = "%d/%m/%Y"), origin = as.Date("1991-01-01")))
Gannets2$G     <- Gannets2$Gannets_in_transect
Gannets2$GProp <- Gannets2$G / Gannets2$Area



# ------------------------------------------------------------------------------
# Fit Poisson GAM and assess overdispersion
# ------------------------------------------------------------------------------

# Poissson GAM
M1 <- gam(G ~ s(Year) + offset(LArea), family = poisson, data = Gannets2)

E1 <- resid(M1, type = "pearson")

Overdispersion1 <- sum(E1^2) / M1$df.res

Overdispersion1



# -->
# There is considerable overdispersion: 36.93



# ----------
# contain a smoothing function for Year and use SeaState as a categrocial variable
M2 <- gam(G ~ s(Year) + factor(Seastate)+ offset(LArea), family = poisson, data = Gannets2)

E2 <- resid(M2, type = "pearson")

Overdispersion2 <- sum(E2^2) / M2$df.res

Overdispersion2



# -->
# The overdispersion is reduced to 29.77



# ----------
# TimeH
M3 <- gam(G ~ s(Year) + factor(Seastate) + s(TimeH) + offset(LArea),  family = poisson, data = Gannets2)

E3 <- resid(M3, type = "pearson")

Overdispersion3 <- sum(E3^2) / M3$df.res

Overdispersion3



# -->
# The overdispersion is reduced to 26.71



# ----------
# DayInYear
M4 <- gam(G ~ s(Year) + factor(Seastate) + s(TimeH) + s(DayInYear) + offset(LArea),  family = poisson, data = Gannets2)

E4 <- resid(M4, type = "pearson")

Overdispersion4 <- sum(E4^2) / M4$df.res

Overdispersion4



# -->
# The overdispersion is reduced to 23.05


