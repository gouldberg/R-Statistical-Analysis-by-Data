setwd("//media//kswada//MyFiles//R//spiders")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Spiders
# ------------------------------------------------------------------------------

Spiders <- read.table(file = "Spiders.txt", header = TRUE, dec = ".")


str(Spiders)



# ----------
# Some plots are dropped from the analysis
Spiders$fPlot <- factor(Spiders$Plot)

Spiders <- Spiders %>% filter(! fPlot %in% c("4", "9", "11", "14", "23"))

Spiders$fPlot <- as.factor(as.numeric(Spiders$fPlot))



# ------------------------------------------------------------------------------
# Plot regression line by each factor
# ------------------------------------------------------------------------------

par(mfrow = c(1,1), mar = c(5,5,2,2))


# ----------
# observed
plot(x = Spiders$HerbLayer, y = Spiders$Hlog10, xlab = "Percentage of herb layer", ylab = "Shannon index", cex.lab = 1.5, pch = 16, col = gray(0.8))


# regression line by each factor
PlotLevels <- unique(levels(Spiders$fPlot))

for (i in PlotLevels){
  MinMaxi <- range(Spiders$HerbLayer[Spiders$fPlot == i])
  MyDatai <- data.frame(HerbLayer = seq(from = MinMaxi[1], to = MinMaxi[2], length = 10), fPlot = i)
  Pi <- predict(M1, newdata = MyDatai)
  lines(MyDatai$HerbLayer, Pi, lty = 1, col = "blue", lwd = 1)
}


# regression line of M0 (no Plot effect)
MinMaxi <- range(Spiders$HerbLayer)
MyDatai <- data.frame(HerbLayer = seq(from = MinMaxi[1], to = MinMaxi[2], length=10))
P <- predict(M0, newdata = MyDatai)
lines(MyDatai$HerbLayer, P, lty = 1, col = 1, lwd = 5)

