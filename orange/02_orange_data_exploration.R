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
# Distribution for each variables
# ------------------------------------------------------------------------------

summary(orange[,1:7])



# -->
# the value ranges are not so much different for each variable



psych::describe(orange)




# ------------------------------------------------------------------------------
# Correlation among variables
# ------------------------------------------------------------------------------

round(cor(orange[,1:7]), digits = 3)


psych::pairs.panels(orange[,1:7], method = "spearman", stars = TRUE)




# ------------------------------------------------------------------------------
# Correlation among oranges
# ------------------------------------------------------------------------------

round(cor(t(orange[,1:7])), digits = 3)

psych::pairs.panels(t(orange[,1:7]), method = "spearman", stars = TRUE)



# -->
# Fruvita and Tropicana is close


