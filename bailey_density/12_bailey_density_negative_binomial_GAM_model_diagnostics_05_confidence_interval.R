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
library(mgcv)

DF$fPeriod <- as.factor(DF$Period)
mod.nbgam <- mgcv::gam(TotAbund ~ s(MeanDepth, by = fPeriod) + fPeriod + offset(LogSA), family = nb(), data = DF)

mod_obj <- mod.nbgam




# ------------------------------------------------------------------------------
# Fitted values and 95% point-wise confidence bands
# ------------------------------------------------------------------------------

MeanDepth.1 <- DF$MeanDepth[DF$fPeriod=="1"]
MeanDepth.2 <- DF$MeanDepth[DF$fPeriod=="2"]

MLSA1 <- mean(DF$LogSA[DF$fPeriod=="1"])
MLSA2 <- mean(DF$LogSA[DF$fPeriod=="2"])

ND1 <- data.frame(MeanDepth = seq(min(MeanDepth.1), max(MeanDepth.1), by = 1), fPeriod = "1",  LogSA = MLSA1)
ND2 <- data.frame(MeanDepth = seq(min(MeanDepth.2), max(MeanDepth.2), by = 1), fPeriod = "2",  LogSA = MLSA2)

PM1.1 <- predict(mod_obj,newdata = ND1, se = TRUE)
PM1.2 <- predict(mod_obj,newdata = ND2, se = TRUE)

AllY <- DF$TotAbun
AllX <- DF$MeanDepth

AllPeriod <- DF$Period
AllPch <- c(1,16)[AllPeriod]

AllMD.1 <- ND1$MeanDepth
AllMD.2 <- ND2$MeanDepth

AllFit.1 <- c(PM1.1$fit)
AllFit.2 <- c(PM1.2$fit)

AllSE.1 <- c(PM1.1$se.fit)
AllSE.2 <- c(PM1.2$se.fit)

xyplot(AllY ~ AllX ,
       strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T, x = list(relation = "same"), y = list(relation = "free")),
       xlab = "Mean depth (m)",
       ylab = "Total abundance",
       panel=function(x,y, subscripts,...){
         x1 <- AllMD.1
         x2 <- AllMD.2
         y1 <- AllFit.1
         y2 <- AllFit.2
         se1 <- AllSE.1
         se2 <- AllSE.2
         panel.grid(h=-1, v= 2)
         
         z1.low <- exp(y1 - 1.96*se1)
         z1.up  <- exp(y1 + 1.96*se1)
         z2.low <- exp(y2 - 1.96*se2)
         z2.up  <- exp(y2 + 1.96*se2)
         
         panel.polygon(c(x1,rev(x1)),c(z1.low,rev(z1.up)), col=grey(0.4),border=NULL)
         panel.polygon(c(x2,rev(x2)),c(z2.low,rev(z2.up)), col=grey(0.7), border=NULL)
         panel.points(x,y, col = 1, pch=AllPch[subscripts],cex=1)
         panel.lines(x1,exp(y1),lwd=3,lty=1,col=1)
         panel.lines(x2,exp(y2),lwd=3,lty=1,col=1)
       })



