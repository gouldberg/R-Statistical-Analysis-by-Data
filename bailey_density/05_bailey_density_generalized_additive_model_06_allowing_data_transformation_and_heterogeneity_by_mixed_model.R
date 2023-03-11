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
# Transforming and allowing for heterogeneity
# ------------------------------------------------------------------------------
# It is also possible to apply a transformation on the density data, and, if the residuals still exhibit heterogeneity, use one of the variance
# structures
# However, intuition would suggest that we should avoid a transformation and use only a variance structure to deal with heterogeneity.


# log transformation
DF$L.Dens <- log(DF$Dens)



# ----------
M1 <- gam(Dens ~ s(MeanDepth, by = factor(Period)) + factor(Period), data = DF)

M2 <- gam(SQ.Dens ~ s(MeanDepth, by = factor(Period)) + factor(Period), data = DF)


# Note that this is gamm()
M3 <- gamm(SQ.Dens ~ s(MeanDepth, by = factor(Period)) + factor(Period), weights = varIdent(form =~ 1 | IMD), method = "REML", data = DF)

M4 <- gam(L.Dens ~ s(MeanDepth, by = factor(Period)) + factor(Period), data = DF)



# ----------
MeanDepth.1 <- DF$MeanDepth[DF$Period == 1]
MeanDepth.2 <- DF$MeanDepth[DF$Period == 2]

ND1 <- data.frame(MeanDepth=seq(min(MeanDepth.1),max(MeanDepth.1),by=1),Period=1)
ND2 <- data.frame(MeanDepth=seq(min(MeanDepth.2),max(MeanDepth.2),by=1),Period=2)



# ----------
PM1.1 <- predict(M1, newdata = ND1, se = TRUE)
PM1.2 <- predict(M1, newdata = ND2, se = TRUE)

PM2.1 <- predict(M2, newdata = ND1, se = TRUE)
PM2.2 <- predict(M2, newdata = ND2, se= TRUE)

PM3.1 <- predict(M3$gam, newdata = ND1, se = TRUE)
PM3.2 <- predict(M3$gam, newdata = ND2, se = TRUE)

PM4.1 <- predict(M4, newdata = ND1, se = TRUE)
PM4.2 <- predict(M4, newdata = ND2, se = TRUE)


# ----------
AllY <- c(DF$Dens, DF$SQ.Dens, DF$SQ.Dens, DF$L.Dens)
AllX <- rep(DF$MeanDepth, 4)
MyNames <- c("Gaussian GAM", "Gaussian GAM, square root density", "Gaussian GAM, square root density, varIdent", "Gaussian GAM, log density")

AllID <- rep(MyNames, each = 147)
AllID <- factor(AllID, levels = MyNames)

AllPeriod <- rep(DF$Period,4)
AllPch <- AllPeriod
AllPch[AllPeriod == 1] <- 16
AllPch[AllPeriod == 2] <- 16

AllMD.1 <- c(ND1$MeanDepth, ND1$MeanDepth, ND1$MeanDepth, ND1$MeanDepth)
AllMD.2 <- c(ND2$MeanDepth, ND2$MeanDepth, ND2$MeanDepth, ND2$MeanDepth)

AllFit.1<- c(PM1.1$fit, PM2.1$fit, PM3.1$fit, PM4.1$fit)
AllFit.2<- c(PM1.2$fit, PM2.2$fit, PM3.2$fit, PM4.2$fit)

NewID.1 <- rep(MyNames,each = 4062)
NewID.2 <- rep(MyNames,each = 4033)

AllSE.1 <- c(PM1.1$se.fit, PM2.1$se.fit, PM3.1$se.fit, PM4.1$se.fit)
AllSE.2 <- c(PM1.2$se.fit, PM2.2$se.fit, PM3.2$se.fit, PM4.2$se.fit)



# ----------
xyplot(AllY ~ AllX | AllID,
       strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T, x = list(relation = "same"), y = list(relation = "free")),
       xlab = list(label = "Depth (m)", cex = 1.5),
       ylab = list(label = "Fish density",cex = 1.5),
       panel=function(x,y, subscripts,...){
         ID <- AllID[subscripts][1]
         print(ID)
         x1 <- AllMD.1[NewID.1==ID]
         x2 <- AllMD.2[NewID.2==ID]
         y1 <- AllFit.1[NewID.1==ID]
         y2 <- AllFit.2[NewID.2==ID]
         se1<- AllSE.1[NewID.1==ID]
         se2<- AllSE.2[NewID.2==ID]
         panel.grid(h = -1, v = 2)
         
         #panel.lines(x1,y1+1.96*se1,lwd=3,lty=2,col=1)
         #panel.lines(x1,y1-1.96*se1,lwd=3,lty=2,col=1)
         
         #panel.lines(x2,y2+1.96*se2,lwd=3,lty=2,col=2)
         #panel.lines(x2,y2-1.96*se2,lwd=3,lty=2,col=2)
         z1.low <- y1 - 1.96 * se1
         z1.up  <- y1 + 1.96 * se1
         z2.low <- y2 - 1.96 * se2
         z2.up  <- y2 + 1.96 * se2
         
         panel.polygon(c(x1,rev(x1)),c(z1.low,rev(z1.up)), col="grey65",border=NULL)
         panel.polygon(c(x2,rev(x2)),c(z2.low,rev(z2.up)), col="indianred1", border=NULL)
         panel.points(x,y,col=1,   #AllPeriod[subscripts],
                  pch=AllPch[subscripts],cex=0.7)
         panel.lines(x1,y1,lwd=3,lty=1,col=1)
         panel.lines(x2,y2,lwd=3,lty=1,col=1)
       })


# -->
# M3 is our final selected model, but a detailed model validation is required.
# For example, it contains one influential observation.
# Differences between the two curves become larger if this influential observation is dropped.
# Also, we used all observations, including the site in the southwest corner of the sampling area.
# It may be sensible to drop this observation from the data set before applying the GAM so that we compare equivalent areas in the two time periods.

