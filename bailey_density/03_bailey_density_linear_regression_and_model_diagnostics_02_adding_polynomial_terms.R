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
# applying cubic polynomials
# ------------------------------------------------------------------------------

# To improve the model, we can attempt to include quadratic or cubic terms for depth.
M3 <- lm(Dens ~ poly(MeanDepth,3), data = DF)

print(summary(M3), digits = 2, signif.stars = FALSE)



# ----------
plot(x = DF$MeanDepth, y = DF$Dens, xlab = MyXLab, ylab = MyYLab)

D1 <- data.frame(MeanDepth=seq(from = min(DF$MeanDepth), to = max(DF$MeanDepth), length = 100))

P1 <- predict(M3, newdata = D1)

lines(x = D1$MeanDepth, y = P1, col = 1, lwd = 5)


# -->
# Unfortunately, with cubic polynomials the fit can be poor at certain areas of the gradient, especially for long gradients.
# Again we have negative fitted values and clusters of observations are underfitted, for example at a depth of 4000 m.



# ------------------------------------------------------------------------------
# Model comparison
# ------------------------------------------------------------------------------

anova(M1, M3, test = "Chisq")



# ------------------------------------------------------------------------------
# Pearson residual, Standardized residuals
# ------------------------------------------------------------------------------

residualPlots(M3)


# residualPlots(M1, type = "rstandard")



# -->
# There is a clear violation of homogeneity and also a patten of clustered residuals above zero line and 
# clustered residuals below the zero line, especially around 4000 m.



# ----------
# residuals by group
residualPlots(M3, groups = DF$Year > 1990)



# ----------
# by index
# standardized Pearson residuals
stand.resid <- rstandard(M3, type = "sd.1")

par(mfrow=c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))




# ------------------------------------------------------------------------------
# Non-constant Variacen Score Test
# ------------------------------------------------------------------------------

car::ncvTest(M3)


