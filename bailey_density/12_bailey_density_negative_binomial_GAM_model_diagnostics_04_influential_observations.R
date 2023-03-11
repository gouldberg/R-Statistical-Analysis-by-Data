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
# Model validation:  influential observations
#   - GAMs do not calculate Cook's distance values. However, we can estimate a smoother using all data and a smoother using a data set
#     in which a single observation is dropped in turn.
#     Subtracting the smoothers and taking the sum of the squared differences gives a rough impression of which observations are influential.
# ------------------------------------------------------------------------------

n  <- length(DF$TotAbund)

ID <- 1:n

MD <- expand.grid(MeanDepth = seq(from = min(DF$MeanDepth), to = max(DF$MeanDepth), length = 50), 
                  fPeriod = as.factor(c(1,2)), 
                  LogSA = seq(from = min(DF$LogSA), to = max(DF$LogSA), length = 50))

P <- predict(mod_obj, newdata = MD, type = "terms")

Dif <- vector(length = n)


for (i in 1:n) {
  print(i)
  mod_obj.i <- mgcv::gam(TotAbund ~ s(MeanDepth, by = fPeriod) + fPeriod + offset(LogSA), family = nb(), data = DF, subset = ID[-i])
  P.i <- predict(mod_obj.i, newdata = MD, type = "terms")
  Dif[i] <- sum((P[1:5000] - P.i[1:5000])^2)
}


par(mfrow=c(1,1))
plot(Dif, type = "h",  xlab = "Observation number",  ylab = "Change in smoother")




# ----------
# There are 4 observations that are more influential than the others,

par(mar = c(5,6,3,3))

plot(x = DF$MeanDepth, y = DF$TotAbund, cex = 3*(Dif/max(Dif))^(1/4), pch = 16, cex.lab = 1.5, col = "darkgray")


idx <- order(Dif, decreasing = TRUE)[1:4]

sapply(1:length(idx), function(x) points(DF$MeanDepth[idx[x]], DF$TotAbund[idx[x]], col = "blue", pch=16))


