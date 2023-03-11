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
# Input = Stilt.Maui  Ouput = Coot.Maui  by gam
# ------------------------------------------------------------------------------

# Maui

graphics.off()
par(mfrow = c(1, 1))

acf(Birds_sm, lag = 10, main = "Stilt.Maui")

acf(Birds_cm, lag = 10, main = "Coot.Maui")

astsa::ccf2(Birds_sm, Birds_cm, max.lag = 10, main = "Stilt vs. Coot in Maui", ylab = "CCF")



# ----------
library(mgcv)



rain <- Hawaii$Rainfall

year <- Hawaii$Year



# not include rain  (not significant)
# ( u <- gam(Birds_cm ~ s(as.numeric(year)) + Birds_sm + rain) )

( u <- gam(Birds_cm ~ s(as.numeric(year)) + s(Birds_sm)) )

# ( u <- gam(Birds_cm ~ s(as.numeric(year)) + stats::lag(Birds_cm, -1)) )



summary(u)



# ------------------------------------------------------------------------------
# plot smoothers
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,2))

plot(u, scale = FALSE, shade = TRUE)



# ------------------------------------------------------------------------------
# assess residuals
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(resid(u), type = "o")




# ------------------------------------------------------------------------------
# plot original and its prediction
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))

plot(Birds_cm, type = "l", lty = 2, lwd = 1, col = "black")
lines(predict(u), type = "l", lwd = 2, col = "blue")




