setwd("//media//kswada//MyFiles//R//orange")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orange
# ------------------------------------------------------------------------------

orange <- read.table("orange.csv", header=TRUE, sep=";", dec=".", row.names=1)

str(orange)

dim(orange)


orange



# ------------------------------------------------------------------------------
# Row Proriles
# ------------------------------------------------------------------------------

ddr <- rbind(orange[,1:7], apply(orange[,1:7], 2, sum))

rownames(ddr)[7] <- "Mean Profile"

ddr

round(addmargins(prop.table(as.matrix(ddr), margin = 1), margin = 2), 3)



# ------------------------------------------------------------------------------
# Column Proriles
# ------------------------------------------------------------------------------

ddc <- cbind(orange[,1:7], apply(orange[,1:7], 1, sum))

colnames(ddc)[8] <- "Mean Profile"

ddc

round(addmargins(prop.table(as.matrix(ddc), margin = 2), margin = 1), 3)




