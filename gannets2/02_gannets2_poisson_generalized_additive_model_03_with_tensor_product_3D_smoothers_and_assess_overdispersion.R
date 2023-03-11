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
# Fit Poisson GAM with 3D smoothers but at fixed degrees of freedom
# ------------------------------------------------------------------------------
# IT TAKES TIME if set k = 10
# Fit Model M6 and put a limit in the Df by 3-dimensional smoother
# 3-dimensional smoother for Year, X, and Y consumes 106 degrees of freedom, which is large.
# We specify an upper limit for the degrees of freedom by adding, k = 3

M6 <- gam(G ~ s(TimeH) + s(DayInYear) + factor(Seastate) + te(Xkm, Ykm, Year, k= 3) + offset(LArea),  family = poisson, data = Gannets2)

E6 <- resid(M6, type = "pearson")

Overdispersion6 <- sum(E6^2) / M6$df.res

Overdispersion6



# ----------
E6 <- resid(M6, type = "pearson")

Overdispersion6 <- sum(E6^2) / M6$df.res

Overdispersion6



# -->
# Overdispersion is reduced to 16.29


# ----------
F6 <- fitted(M6, type = "response")

plot(x = F6, y = E6, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h = 0, v = 0)


# -->
# There are various large residuals.
# We suspect that the large observed values are partly responsible for the overdispersion.


# -->
# The next logical step is to apply the negative binomial equivalent of the model


