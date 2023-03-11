setwd("//media//kswada//MyFiles//R//wine")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wine
#   - 
# ------------------------------------------------------------------------------

wine <- read.table("wine.csv", header = TRUE, row.names = 1, sep = ";", check.names = FALSE)


# this produces error
str(wine)


# data("wine", package = "FactoMineR")

dim(wine)

head(wine)



# ------------------------------------------------------------------------------
# Chi-square independence test
# ------------------------------------------------------------------------------

# test for uniform distribution at final "total" row.

chisq.test(wine[nrow(wine), 1:(ncol(wine)-1)])


# -->
# The p-valuue confirsm that we must not focus on the differences between the numbers of words for each wine.
# In terms of CA, we can consider that the wines will all have around the same influence on the analysis.
# When the number of words per wine differs, the analysis attributes a greater weight to a wine if it is the subject of a great number of commentaries



# ----------
# test for independence of two variables for whole table
chisq.test(wine[1:(nrow(wine)-1), 1:(ncol(wine)-1)])



# -->
# Total inertia = 1.5, and subsequently X^2 of 368.79 (= 245(=n) * 1.5)
# associated with a p-value equal to almost zero.
# This table has poor validity test conditions (in theory, at least 80% of the theoretical sample size must have a value higher than 5 and none of them should be nil.)
# However, the p-value is so low that the worth of CA for this data cannot be questioned.





