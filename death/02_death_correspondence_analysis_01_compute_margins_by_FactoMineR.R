setwd("//media//kswada//MyFiles//R//death")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  death
#   - 
# ------------------------------------------------------------------------------

death <- read.table("death.csv", header = TRUE, row.names = 1, sep = ";")

str(death)

dim(death)

head(death)


colnames(death) <- c("0_1", "1_4", "5_14", "15_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75_84", "85_94", "95+")



# ------------------------------------------------------------------------------
# Row Proriles
# ------------------------------------------------------------------------------

ddr <- rbind(death[1:65,], apply(death[1:65,], 2, sum))

rownames(ddr)[66] <- "Mean Profile"

ddr

round(addmargins(prop.table(as.matrix(ddr), margin = 1), margin = 2), 3)[66,]



# ------------------------------------------------------------------------------
# Column Proriles
# ------------------------------------------------------------------------------

ddc <- cbind(death[1:65,], apply(death[1:65,], 1, sum))

colnames(ddc)[13] <- "Mean Profile"

ddc

round(addmargins(prop.table(as.matrix(ddc), margin = 2), margin = 1), 3)[,13]




# ------------------------------------------------------------------------------
# Calculate row profiles and column profiles (margins) by CA
# ------------------------------------------------------------------------------


# Correspondence analysis CA first by "graph = FALSE"
res.ca <- CA(death, row.sup = 66:nrow(death), graph = FALSE)



# ----------
# row profiles (age group margins)
round(res.ca$call$marge.col, 3)
round(addmargins(prop.table(as.matrix(ddr), margin = 1), margin = 2), 3)[66,]



# ----------
# column profiles (age group margins)
round(res.ca$call$marge.row, 3)
round(addmargins(prop.table(as.matrix(ddc), margin = 2), margin = 1), 3)



# ----------
par(las = 1, mfrow = c(1,2))

barplot(res.ca$call$marge.col, horiz = TRUE)

barplot(res.ca$call$marge.row[order(res.ca$call$marge.row)], horiz = TRUE)

par(las = 0)




# -->
# The margin designate the weights of each category of the CA.
# The two margins vary greatly.

# The most frequent cause of death is linked to cerebrovascular disease.
# The age group with the highest number of deaths is the range between 75 and 84 years.
# The higher age groups (85 - 94 years and 95 years and other) feature fewer deaths simply because there are far fewer people of this age.
# It may also be noted that the number of deaths in the lowest age group (0 - 1 year)
# is relatively high when compared to the next age groups.
# This is even more surprising as this age group includes individuals all born in the sam year,
# whereas the other groups include groups with a range of 4 and then 10 years.
# The percentage of children 0 - 1 year who die is therefore much higher tahn the percentage for children 1 - 4 years or 5 - 14 years.

