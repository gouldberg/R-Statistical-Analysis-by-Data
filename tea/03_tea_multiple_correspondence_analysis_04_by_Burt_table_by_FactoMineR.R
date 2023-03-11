setwd("//media//kswada//MyFiles//R//tea")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tea
#  - survey of 300 tea drinkers for tea-drinking behaviour (19 questions), the image they have of the product (12 questions) and 4 descriptive quesionts
# ------------------------------------------------------------------------------

tea <- read.table("tea.csv", header = TRUE, sep = ";")

dim(tea)

str(tea)


car::some(tea)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR from Burt Table
#  - Correspondence analysis of this table is used to represent the categories.
#    As this table is symmetrical, the representation of the cloud of row profiles is identical to that of the cloud of column profiles.
#    (only one of the two representations is therefore retained)
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(tea, method = "Burt", quanti.sup = 22, quali.sup = c(19:21, 23:36))

summary(res.mca)



# -->
# This representation is very similar to the representation of the categories as provided by MCA and demonstrates
# the collinearity of the principal components of the same rank.

# However, the iinertias associated with each component differ.
# When lambda_s is the inertia of rank s for the MCA, the inertia of component rank s for a CA of the Burt table will be lambda_s^2

# Here the percentages of inertia associated with the first two components of the MCA are worth 9.88% and 8.10%,
# respectively, compared with 20.73% and 14.11% for those of the CA


