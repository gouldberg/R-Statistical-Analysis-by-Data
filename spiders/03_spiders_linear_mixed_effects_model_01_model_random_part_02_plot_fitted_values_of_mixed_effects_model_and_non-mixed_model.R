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
# plot M1 and M2 (linear mixed effects model)
# ------------------------------------------------------------------------------

par(mfrow = c(1,2), mar = c(5,5,2,2))


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



# ----------
MinMax <- range(Spiders$HerbLayer)
MyData <- data.frame(HerbLayer = seq(from = MinMax[1], to = MinMax[2], length = 10))

X   <- model.matrix(~ HerbLayer, data = MyData)
Fit <- X %*% fixef(M2)

plot(x = Spiders$HerbLayer, y = Spiders$Hlog10, xlab = "Percentage of herb layer", ylab = "Shannon index", cex.lab = 1.5, pch = 16, col = gray(0.8))
lines(MyData$HerbLayer, Fit, lwd = 5)

j <- 1
a <- ranef(M2)$fPlot$'(Intercept)'

PlotLevels <- unique(Spiders$fPlot)
for (i in PlotLevels){
  MinMaxi <- range(Spiders$HerbLayer[Spiders$fPlot==i])
  MyDatai <- data.frame(HerbLayer = seq(from = MinMaxi[1], to = MinMaxi[2], length=10), fPlot = i)
  X <- model.matrix(~ HerbLayer, data = MyDatai)
  Pi <- X %*% fixef(M2) + a[j]
  j <- j + 1
  lines(MyDatai$HerbLayer, Pi, lty=1, col = "red", lwd = 1)
}

