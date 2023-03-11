setwd("//media//kswada//MyFiles//R//death")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  death
# ------------------------------------------------------------------------------

death <- read.table("death.csv", header = TRUE, row.names = 1, sep = ";")

str(death)

dim(death)

head(death)


colnames(death) <- c("0_1", "1_4", "5_14", "15_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75_84", "85_94", "95+")



# ------------------------------------------------------------------------------
# Decompose inertia by row and by column
# ------------------------------------------------------------------------------

res.ca = CA(death, row.sup = 66:nrow(death), graph = FALSE)

summary(res.ca, nb.dec = 4)



# ----------
# Decompose inertia by column
sum(res.ca$col$inertia)
100 * res.ca$col$inertia / sum(res.ca$col$inertia)


# -->
# The inertia for the age group 0-1 year is high, as 52.6% of the total inertia is due to this age group.
# Half of the relationship between age and cause of death therefore resides in the characteristic of this age group,
# which will therefore have a strong influence on the results of the CA.
# The two other age groups which contribute the most to this relationship are 15 - 24 years and 25 - 34 years.
# These age ranges have very specific mortality profiles and will also strongly influence the CA.



# ----------
# Decompose inertia by row in descending order
100 * res.ca$row$inertia[rev(order(res.ca$row$inertia))] / sum(res.ca$row$inertia)


# -->
# For the 65 causes of death, we here list those with the five highest inertias (in the entire space), in descending order
# Perinatal infection has high inertia compared with the other causes of mortality (32.41%);
# however, its weight is relatively low (with a margin og 0.00336)
# This cause of death has a very specific age profile (as suggested by the name).



# ------------------------------------------------------------------------------
# weight, distance from the origin, and inertia (row)
# ------------------------------------------------------------------------------
# The details of the calculations of these inertias in the form of a table summarising the weight (equal to the margin expressed as a percentage),
# the distance from the origin, and the inertia (raw, and as apercentag) for each row and each column.

bb <- round(cbind.data.frame(res.ca$call$marge.col, 
                             sqrt(res.ca$col$inertia / res.ca$call$marge.col), 
                             res.ca$col$inertia, 
                             res.ca$col$inertia / sum(res.ca$col$inertia)), 4)

colnames(bb) <- c("Weight", "Distance", "Inertia", "% of inertia")


bb


# -->
# It would therefore seem that the strong contribution of the age group 15 to 24 years stems primarily from the distance from the origina,
# and therefore is a highly specific mortality profile.



