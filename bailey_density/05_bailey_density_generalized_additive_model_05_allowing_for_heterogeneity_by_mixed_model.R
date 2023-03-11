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
# Allowing for heterogeneity
# ------------------------------------------------------------------------------

# based on the basic analysis (but subjective), residuals at sites with depth < 2000 m 
# have larger variation than residuals from sites at greater depth.

IMD <- DF$MeanDepth

IMD[DF$MeanDepth < 2000] <- 1

IMD[DF$MeanDepth >= 2000] <- 2

IMD <- factor(IMD)



# ---------
# note that we use not gam but gamm()
# The model is fitted with the function gamm, which stands for generalised additive mixed modelling,
# though technically this is not a mixed effects model.

M11 <- gamm(Dens ~ s(MeanDepth, by = fPeriod) + fPeriod, weights = varIdent(form =~ 1 | IMD), data = DF)

summary(M11$lme)



# -->
# the parametrisation is 1.0 * sigma for IMD = 1 and 0.1348 * sigma for IMD = 2
# --> variance is (1.0 * sigma)^2 and (0.1348 * sigma)^2

summary(M11$gam)




# -->
# sigma (standard deviaion) is obtained from scale estimated
# Some of the estimated values are small, due to depth being expressed in metres.
# It is advisable to express depth in kilometeres to avoid numerical estimation problems,
# especially if depth is used in variance functions.



# ----------
# standardized residuals vs. fitter values
# standardized residuals = residuals / sqrt( sigma-hat^2 )
# if we assume that e ~ N(0, sigma^2 * |Depth|^(2*sigma)), standardized residual = residual / sqrt( sigma^2 * |Depth|^(2*sigma) )

plot(M11$lme, col = 1, pch = 16, cex.lab = 1.5)


# -->
# considerably better (no patterns) than the model without the VarIdent structure


