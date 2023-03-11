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
# Fit Negative Binomial GAM
#   - The negative binomial GAM uses a quadratic variance
#     G(i) ~ NB(mu(i), k)
#     E(G(i)) = mu(i)  and  var(G(i)) = mu(i) + mu(i)^2 / k  (k is called theta in R's code)
# ------------------------------------------------------------------------------

# M8 <- gam(G ~ s(Year) + te(TimeH, DayInYear) + factor(Seastate) + offset(LArea) + te(Xkm, Ykm), family = negbin(c(0.1, 1.5), link = log), data = Gannets2)
# M8 <- gam(G ~ s(Year) + te(TimeH, DayInYear) + factor(Seastate) + offset(LArea) + te(Xkm, Ykm), family = nb(c(0.1, 1.5), link = log), data = Gannets2)
M8 <- gam(G ~ s(Year) + te(TimeH, DayInYear) + factor(Seastate) + offset(LArea) + te(Xkm, Ykm), family = nb(0.1), link = log, data = Gannets2)

E8 <- resid(M8, type = "pearson")


Overdispersion8 <- sum(E8^2) / M8$df.res


Overdispersion8



# ----------
print(summary(M8), digits = 3, signif.stars = FALSE)

anova(M8)


# -->
# Condensed output by anova()
# All terms in the model are significant at the 5% level.
# UBRE was used to estimate the optimal amount of smoothing for each smoother



# ----------
# A plot of the Pearson residuals versus fitted values.
# The filled dark dots are Pearson residuals for which the observed gannet abundance is greater than 25.
par(mfrow = c(1,1), mar = c(5,5,3,3))
E8 <- resid(M8, type = "pearson")
F8 <- fitted(M8, type = "response")

MyPch <- rep(1, length = nrow(Gannets2))
MyPch[Gannets2$G > 25] <- 16
MyCex <- rep(0.5, length = nrow(Gannets2))
MyCex[Gannets2$G > 25] <- 1.2

MyCol <- rep(grey(0.4), length = nrow(Gannets2))
MyCol[Gannets2$G > 25] <- grey(0.2)

plot(x = F8, y = E8, xlab = "Fitted values", ylab = "Pearson residuals", cex.lab = 1.5, pch = MyPch, cex = MyCex, col = MyCol)
abline(h = 0, v = 0)


# -->
# It seems that the model is not capable of fitting the observations with high abundance.
# Besides the large residuals, the excessive number of zeros may also have caused some of the overdispersion.



# ------------------------------------------------------------------------------
# Check smoothers
# ------------------------------------------------------------------------------

# The Year smoother shows a decrease to 1993 and an increase from 1993 to 1997 followed by a sharp decrease and another increase.
plot(M8, select = c(1), cex.lab = 1.5)



# The te(DayInYear, TimeH) smoother (daily and seasonal patterns) is difficult to interpret, but it clearly shows a change in the daily pattern
# during the season.
plot(M8, select = c(2), cex.lab = 1.5, scheme = TRUE)



# Spatial smoother shows that higher abundances are obtained at transects in the south-western portion of the dtudy area.
plot(M8, select = c(3), cex.lab = 1.5, scheme = TRUE)



