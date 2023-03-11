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

# Stilt.Maui and Coot.Maui

Birds <- c(Hawaii$Stilt.Maui, Hawaii$Coot.Maui)


Time <- rep(Hawaii$Year, 2)


Rain <- rep(Hawaii$Rainfall, 2)


ID <- factor(rep(c("Stilt.Maui", "Coot.Maui"), each = length(Hawaii$Year)))



# ----------
xyplot(Birds ~ Time | ID, col = 1, type = "o")



# ----------
Birds_sm <- Hawaii$Stilt.Maui

Birds_cm <- Hawaii$Coot.Maui


Birds_so <- Hawaii$Stilt.Oahu

Birds_co <- Hawaii$Coot.Oahu



# ------------------------------------------------------------------------------
# Regression with lagged variables
# Transfer function model
# ------------------------------------------------------------------------------

summary(lm(Birds_sm ~ time(Birds_sm)))

        

# Detrended Stilt.Maui
sm.d <- resid(lm(Birds_sm ~ time(Birds_sm), na.action = NULL))


acf2(sm.d)




# ----------
# sm.d: detrended

birds <- ts.intersect(cm = Birds_cm, sm = Birds_sm, cm1 = stats::lag(Birds_cm, -1))

birds <- ts.intersect(cm = Birds_cm, sm = sm.d, cm1 = stats::lag(Birds_cm, -1))


# only take 2nd variable for explanatory variable  (not lag)
( u <- lm(birds[,1] ~ birds[,2], na.action = NULL) )


summary(u)



acf2(resid(u))




# ----------
# final model
arx <- sarima(birds[,1], 1, 0, 0, xreg = birds[,2])


arx



# ------------------------------------------------------------------------------
# 1-step-ahead predictions
# ------------------------------------------------------------------------------

pred <- Birds_cm + resid(arx$fit)


par(mfrow = c(1,1))

ts.plot(pred, Birds_cm, col = c('skyblue', "black"), lwd = c(7,2))


