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
# Fit Poisson GAM with 2-dimentional smoothers (tensor product smoother)
# ------------------------------------------------------------------------------
# Obviously, each covariate in a 2-dimensional smoother should be equally important in determining the shape of the 2-dimensional smoother,
# and, therefore, a so-called non-isotropic smoother should be used, for example the tensor product smoother.
# The tensor product smoother is useful if the covariates are expressed in different units of have a different scale.
# However, it is inferior to the thin plate regression spline for well-scaled covariates.
# Our study area is more rectangular than square, and, although the spatical covariates use the same units (unlike latitude and longitude),

# we should perhaps use te(X, Y) instead os s(X, Y), which uses the default thin plate regression spline.

M5 <- gam(G ~ s(Year) + s(TimeH) + s(DayInYear)+ te(Xkm, Ykm) + factor(Seastate) + offset(LArea), family = poisson, data = Gannets2)

E5 <- resid(M5, type = "pearson")

Overdispersion5 <- sum(E5^2) / M5$df.res

Overdispersion5


# -->
# Overdispersion is reduced to 20.08


# ----------
F5 <- fitted(M5, type = "response")

plot(x = F5, y = E5, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h = 0, v = 0)




# ----------
# Fit Model M7 with two 2-dimentional smoothers
M7 <- gam(G ~ s(Year) + factor(Seastate) + te(TimeH, DayInYear) + te(Xkm, Ykm) + offset(LArea),  family = poisson, data = Gannets2)

E7 <- resid(M7, type = "pearson")

Overdispersion7 <- sum(E7^2) / M7$df.res

Overdispersion7


# -->
# Overdispersion is reduced to 17.84
