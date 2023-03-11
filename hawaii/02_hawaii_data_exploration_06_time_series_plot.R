# setwd("//media//kswada//MyFiles//R//hawaii")
# setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

# Other than Moorhen.Kaui

Birds <- c(Hawaii$Stilt.Oahu, Hawaii$Stilt.Maui, Hawaii$Coot.Oahu, Hawaii$Coot.Maui, Hawaii$Moorhen.Kauai)


Time <- rep(Hawaii$Year, 5)


Rain <- rep(Hawaii$Rainfall, 5)


ID <- factor(rep(c("Stilt.Oahu", "Stilt.Maui", "Coot.Oahu", "Coot.Maui", "Moorhen.Kauai"), each = length(Hawaii$Year)))



# ----------
xyplot(Birds ~ Time | ID, col = 1, type = "o")


# -->
# Some species have considerable more variation, indicating violation of homogeneity



# ----------
xyplot(Birds ~ Rain | ID, col = 1, type = c("p", "smooth"), grid = TRUE)



# -->
# Coot.Maui, Coot.Oahu, Moorhen.Kauai:  Small rainfall and large birds


