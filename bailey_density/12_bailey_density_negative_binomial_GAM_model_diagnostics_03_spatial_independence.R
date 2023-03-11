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

mod.nbgam <- mgcv::gam(TotAbund ~ s(MeanDepth, by = fPeriod) + fPeriod + offset(LogSA), family = nb(), data = DF)

mod_obj <- mod.nbgam




# ------------------------------------------------------------------------------
# Model validation:  independence (violation by spatial dependence)
# ------------------------------------------------------------------------------

# Check a clustering of points of a particular size.
# Bubble plot of the residuals:  Grey and black circles correspond to negative and positive residuals, respectively.

res <- resid(mod_obj)


MyCex <- abs(res) / max(abs(res))
MyCol <- vector(length = length(res))
MyCol[res > 0]  <- gray(0.5)
MyCol[res <= 0] <- gray(0.2)

xyplot(Ykm ~ Xkm, data = DF,
       main = list(label = "Residuals", cex = 1.5),
       xlab = list(label = "X-coordinates", cex = 1.5),
       ylab = list(label = "Y-coordinates", cex = 1.5),
       aspect = "iso", pch = 16, col = MyCol, cex = 3 * (MyCex)^(1/6))



# ------------------------------------------------------------------------------
# Model validation:  sample semi-variogram
# ------------------------------------------------------------------------------

library(sp)
library(gstat)

mydata <- data.frame(res, DF$Xkm, DF$Ykm)

coordinates(mydata) <- c("DF.Xkm","DF.Ykm")

Vari <- variogram(res ~ 1, mydata)

plot(Vari)

