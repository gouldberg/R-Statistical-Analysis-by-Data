setwd("//media//kswada//MyFiles//R//jo")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  JO
# ------------------------------------------------------------------------------

data("JO", package = "FactoMineR")

str(JO)

dim(JO)


head(JO)



# ------------------------------------------------------------------------------
# Row Proriles
# ------------------------------------------------------------------------------

ddr <- rbind(JO, apply(JO, 2, sum))

rownames(ddr)[25] <- "Mean Profile"

ddr

round(addmargins(prop.table(as.matrix(ddr), margin = 1), margin = 2), 3)



# ------------------------------------------------------------------------------
# Column Proriles
# ------------------------------------------------------------------------------

ddc <- cbind(JO, apply(JO, 1, sum))

colnames(ddc)[59] <- "Mean Profile"

ddc

round(addmargins(prop.table(as.matrix(ddc), margin = 2), margin = 1), 3)




