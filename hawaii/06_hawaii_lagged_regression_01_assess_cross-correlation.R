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
# Cross correlation among different species in same island
# ------------------------------------------------------------------------------

# Maui

graphics.off()
par(mfrow = c(1, 1))

# acf(Birds_sm, lag = 10, main = "Stilt.Maui")

# acf(Birds_cm, lag = 10, main = "Coot.Maui")

astsa::ccf2(Birds_sm, Birds_cm, max.lag = 10, main = "Stilt vs. Coot in Maui", ylab = "CCF")



# -->
# lag at 0 and -1:  cross correlation is large





# ----------
# but not for Oahu:  note the id 5 and 39 are missing

graphics.off()
par(mfrow = c(3, 1))

astsa::acf2(Birds_so, max.lag = 10, main = "Stilt.Maui")

astsa::acf2(Birds_co, max.lag = 10, main = "Coot.Maui")

astsa::ccf2(Birds_so, Birds_co, max.lag = 10, main = "Stilt vs. Coot in Oahu", ylab = "CCF")




# ----------
astsa::lag2.plot(Birds_sm, Birds_cm, max.lag = 12)

