# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)


# ----------
Hawaii$Birds <- sqrt(Hawaii$Moorhen.Kauai)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

# Other than Moorhen.Kaui

Birds <- c(Hawaii$Stilt.Oahu, Hawaii$Stilt.Maui, Hawaii$Coot.Oahu, Hawaii$Coot.Maui)


Time <- rep(Hawaii$Year, 4)


Rain <- rep(Hawaii$Rainfall, 4)


ID <- factor(rep(c("Stilt.Oahu", "Stilt.Maui", "Coot.Oahu", "Coot.Maui"), each = length(Hawaii$Year)))



# ----------
xyplot(Birds ~ Time | ID, col = 1, type = "o")


# -->
# Some species have considerable more variation, indicating violation of homogeneity



# ------------------------------------------------------------------------------
# Smoother for each time series and different residual spread for each time series + corAR1 correlation structure
#   - Birds(is) = alpha(i) + beta + Rainfall(is) + f(i)(Year(s)) + error(is)
#   - error(is) = phi * error(i,s-1) + eta(is)
# ------------------------------------------------------------------------------

library(mgcv)


# Auto-correlation is applied on each individual time series
BM2 <- gamm(Birds ~ Rain + ID +
              s(as.numeric(Time), by = ID), weights = varIdent(form = ~1 | ID), correlation = corAR1(form = ~ Time | ID))



# ----------
summary(BM2$gam)


anova(BM2$gam)



# -->
# Oahu time series have a significant long-term trend and rainfall effect,
# whereas the Maui time series are only affected by rainfall.



# ----------
summary(BM2$lme)



# -->
# phi1 = 0.32, large enough to keep it in the model



# ------------------------------------------------------------------------------
# plot smoothers
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))

plot(BM2$gam, scale = FALSE, shade = TRUE)


# for comparison
plot(BM1$gam, scale = FALSE, shade = TRUE)



# -->
# The long-term trend for Stils on Oahu is linear, but the Coots on Oahu show a non-linear trend over time.
# Abundances are increasing from the early 1970s onwards.




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

AIC(BM1$lme, BM2$lme)



# -->
# The AIC for the model with auto-correlation is better



# ------------------------------------------------------------------------------
# Assess residual pattern:  Normalised residuals
# ------------------------------------------------------------------------------

E2 <- residuals(BM2$lme, type = "normalized")


EAll <- vector(length = length(Birds))

EAll[] <- NA

I1 <- !is.na(Birds)

EAll[I1] <- E2


xyplot(EAll ~ Time | ID, col = 1, ylab = "Residuals", type = "o")



# -->
# The residual spread for the stilt at Oahu are perfect, but the residual spread for the stilts at Maui show a clear increase.



# ------------------------------------------------------------------------------
# Auto-correlation of normalized residuals
# ------------------------------------------------------------------------------


E1 <- EAll[ID == "Stilt.Oahu"]

E2 <- EAll[ID == "Stilt.Maui"]

E3 <- EAll[ID == "Coot.Oahu"]

E4 <- EAll[ID == "Coot.Maui"]



# ----------
par(mfrow = c(2,2))

acf(E1, na.action = na.pass)

acf(E2, na.action = na.pass)

acf(E3, na.action = na.pass)

acf(E4, na.action = na.pass)



# -->
# No problem for auto-correlation within each ID



# ------------------------------------------------------------------------------
# Correlation of residuals among species
# ------------------------------------------------------------------------------

D <- cbind(E1, E2, E3, E4)


cor(D, use = "pairwise.complete.obs")



# -->
# All correlation coefficients are smaller than 0.2, except for the correlation coefficient between stils and coots on Maui (= 0.46)
# This may indicate that the model is missing an important covariate for the Maui time series.
# The 3 options are
#  - (i) find the missing covariate and put it into the model
#  - (ii) extend the residual correlation structure by allowing for the correlation
#  - (iii) ignore the problem because it is only one out of the six correlations, and all p-values in the model were rather small

# If more than one correlation has a high values, option (iii) should NOT be considered.
# You could try programming your own correlation structure allowin for spatial AND temporal correlation


