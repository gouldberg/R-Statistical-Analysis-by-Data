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
# Data Exploration:  Visualize spatial location by year
# ------------------------------------------------------------------------------

xyplot(Ykm ~ Xkm | Year, aspect = "iso", col = 1, 
       data = DF, xlab = "X-coordinate",  ylab = "Y-coordinate")



# ------------------------------------------------------------------------------
# Data Exploration:  Visualize spatial location before 1990 and after 1990
# ------------------------------------------------------------------------------

DF$MyPch0 <- DF$Year

DF$MyPch0[DF$Year < 1990] <- 1

DF$MyPch0[DF$Year > 1990] <- 16



# ----------
xyplot(Ykm ~ Xkm, aspect = "iso", col = 1, 
       data = DF, xlab = "X-coordinate",  ylab = "Y-coordinate", pch = DF$MyPch)




# ------------------------------------------------------------------------------
# relationship between fish density and mean sampling depth
# ------------------------------------------------------------------------------

MyXLab <- "Mean sampling depth (m)"

MyYLab <- expression(paste("Fish density (",m^{-2}, ")"))

par(mfrow=c(1,1), mar = c(5,5,2,3))
plot(x = DF$MeanDepth, y = DF$Dens, xlab = MyXLab, ylab = MyYLab, col= DF$Period, pch = DF$MyPch0, cex.lab = 1.5)


# -->
# scatterplot of the data clearly shows a non-linear pattern.
# Note that the variation decreases at greater depth.



