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
# Add time series correlation structures
#   - Exponential correlation:  corExp
#   - Gaussian correlation:  corGaus
#   - Linear correlation:  corLin
#   - Rational quadratic correlation:  corRatio
#   - Spherical correlation:  corSpher
# ------------------------------------------------------------------------------

library(mgcv)


library(nlme)



f1 <- formula(Birds ~ Rain + ID + s(as.numeric(Time), by = ID))


HawA <- gamm(f1, method = "REML", correlation = corAR1(form = ~ Time | ID), weights = varIdent(form = ~1 | ID))


HawB <- gamm(f1, method = "REML", correlation = corLin(form = ~ Time | ID, nugget = TRUE), weights = varIdent(form = ~1 | ID))


HawC <- gamm(f1, method = "REML", correlation = corGaus(form = ~ Time | ID, nugget = TRUE), weights = varIdent(form = ~1 | ID))


# corExp structure is closely related to the corAR1
HawD <- gamm(f1, method = "REML", correlation = corExp(form = ~ Time | ID, nugget = TRUE), weights = varIdent(form = ~1 | ID))


# corSpher can cope better with missing values and irregulaly spaced data
HawE <- gamm(f1, method = "REML", correlation = corSpher(form = ~ Time | ID, nugget = TRUE), weights = varIdent(form = ~1 | ID))





# ----------
AIC(HawA$lme, HawB$lme, HawC$lme, HawD$lme, HawE$lme)



# -->
# indicate the model with the corAR1 structure should be chosen



# ----------
summary(HawA$lme)



# -->
# phi is 0.391944



# ----------
anova(HawA$lme, HawE$lme)




# ------------------------------------------------------------------------------
# plot smoothers for comparison
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))

plot(HawA$gam, scale = FALSE, shade = TRUE)

plot(HawE$gam, scale = FALSE, shade = TRUE)




