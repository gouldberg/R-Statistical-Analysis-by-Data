setwd("//media//kswada//MyFiles//R//gannets2")

packages <- c("dplyr", "lattice", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gannets2
# ------------------------------------------------------------------------------

Gannets <- read.table(file = "Gannets2.txt", header = TRUE)


str(Gannets)
names(Gannets)


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
# Try two 2-dimensional tensor product smoothers  --> IT TAKES TIME !!!!
# ------------------------------------------------------------------------------
library(gamlss)
library(gamlss.add)



# ----------
# Try two 2-dimensional tensor smoothers  --> IT TAKES TIME !!!!
M11 <- gamlss(G ~  cs(Year, df = 8) +
                ga(~te(TimeH, DayInYear, fx = TRUE, k = 28))+
                ga(~s(Xkm, Ykm, fx = TRUE, k = 28)) +
                factor(Seastate) + offset(LArea),
              family = ZIP(), data = Gannets2)



# ----------
Pi <- fitted (M10, "sigma")[1]
Pi

pi   <- M10$sigma.fv[1]
mu   <- M10$mu.fv
ExpY <- (1 - pi) * mu
varY <-  mu * (1 - pi) * (1 + mu * pi)
PearsonRes <- (Gannets2$G - ExpY) / sqrt(varY)
N    <- nrow(Gannets2)
p    <- M10$df.fit

Overdispersion <- sum(PearsonRes^2) / (N - p)

Overdispersion


# -->
# The next step is to use a zero-inflated negative binomaial GAM to reduce the overdispersion of 3.33 obtained by the negative binomial GAM.
# The function gamlss is not currently capable of executing a zero-inflated negative binomial distribution.
