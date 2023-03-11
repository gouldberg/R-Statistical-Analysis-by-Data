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
# Data exploration:  multi-panel Cleveland dotplot
# ------------------------------------------------------------------------------

Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)), groups=FALSE,
               strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE), y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}


# ----------
graphics.off()

MyVar1 <- c("Site", "Year", "Period", "Xkm", "Ykm")

Mydotplot(as.matrix(DF1[,MyVar1]))



# -->
# Year is around 1980 and 2000 to 2002



# ----------
graphics.off()

MyVar2 <- c("TotAbund", "Dens", "MeanDepth", "SweptArea")

Mydotplot(as.matrix(DF1[,MyVar2]))



# -->
# Some values of TotAbund and Dens are very close to zero



# ------------------------------------------------------------------------------
# Total abundance and swept sea 
# ------------------------------------------------------------------------------

DF$fPeriod <- factor(DF$Period)



# ----------
par(mfrow = c(2,2), mar = c(5,5,2,2))

dotchart(DF$SweptArea, cex.lab = 1.5, xlab = "Swept area", ylab = "Order of the data")

dotchart(DF$TotAbund, cex.lab = 1.5, xlab = "Total abundance", ylab = "Order of the data")

plot(x = DF$MeanDepth, y = DF$SweptArea, xlab = "Mean depth (m)", ylab = "Swept area", cex.lab = 1.5)

plot(x = DF$MeanDepth, y = DF$TotAbund, xlab = "Mean depth (m)", ylab = "Total abundance", cex.lab = 1.5)



# -->
# Cleveland dotplots of total abundance (count) and swept area as well as scatterplots of these two variables versus depth.
# Note that swept area changes considerable between the trawls (the points represent individual overvations)


