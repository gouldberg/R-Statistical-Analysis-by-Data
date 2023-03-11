setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# Row Proriles
# ------------------------------------------------------------------------------

ddr <- rbind(temperature[1:23,1:12], apply(temperature[1:23,1:12], 2, sum))

rownames(ddr)[24] <- "Mean Profile"

ddr

round(addmargins(prop.table(as.matrix(ddr), margin = 1), margin = 2), 3)



# ------------------------------------------------------------------------------
# Column Proriles
# ------------------------------------------------------------------------------

ddc <- cbind(decathlon[1:23,1:12], apply(decathlon[1:23,1:12], 1, sum))

colnames(ddc)[13] <- "Mean Profile"

ddc

round(addmargins(prop.table(as.matrix(ddc), margin = 2), margin = 1), 3)




