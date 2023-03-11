# setwd("//media//kswada//MyFiles//R//orange")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\orange")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orange
#   - The six orange juices were evaluated by a panel of experts according to 7 sensory variable:
#     odour intensity, odour typicality, pulp content, intensity of taste, acidity, bitterness, sweetness
# ------------------------------------------------------------------------------

orange <- read.table("orange.csv", header=TRUE, sep=";", dec=".", row.names=1)

str(orange)

dim(orange)


orange



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
