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
M5 <- mgcv::gam(Dens ~ s(MeanDepth), data = DF)
M3 <- gamm(SQ.Dens ~ s(MeanDepth, by = factor(Period)) + factor(Period), weights = varIdent(form =~ 1 | IMD), method = "REML", data = DF)


mod_obj <- M5
mod_obj <- M3$gam



# ------------------------------------------------------------------------------
# Model validation:  influential observations
#   - GAMs do not calculate Cook's distance values. However, we can estimate a smoother using all data and a smoother using a data set
#     in which a single observation is dropped in turn.
#     Subtracting the smoothers and taking the sum of the squared differences gives a rough impression of which observations are influential.
# ------------------------------------------------------------------------------

n  <- length(DF$Dens)

ID <- 1:n

MD <- data.frame(MeanDepth = seq(from = min(DF$MeanDepth), to = max(DF$MeanDepth), length = 100))

P <- predict(mod_obj, newdata = MD, type = "terms")


P


Dif <- vector(length = n)


for (i in 1:n) {
  print(i)
  mod_obj.i <- mgcv::gam(Dens ~ s(MeanDepth), data = DF, subset = ID[-i])
  P.i <- predict(mod_obj.i, newdata = MD, type = "terms")
  Dif[i] <- sum((P[1:100] - P.i[1:100])^2)
}


par(mfrow=c(1,1))
plot(Dif, type = "h",  xlab = "Observation number",  ylab = "Change in smoother")



# ----------
# There are 2 or 3 observations that are more influential than the others,
# but we can not say whether the differences between the smoothers are significant or important.

par(mar = c(5,6,3,3))

plot(x = DF$MeanDepth, y = DF$Dens, cex = 3*(Dif/max(Dif))^(1/4), pch = 16, cex.lab = 1.5, col = "darkgray")


idx <- c(1,2,3,6)

sapply(1:length(idx), function(x) points(DF$MeanDepth[idx[x]], DF$Dens[idx[x]], col = "blue", pch=16))


