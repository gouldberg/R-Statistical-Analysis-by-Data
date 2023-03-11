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
# Multiple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(tea, quanti.sup = 22, quali.sup = c(19:21, 23:36))

summary(res.mca)



# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a categorical variable
# (i.e., around the barycentre of the individuals carrying that category)
# to assess whether two categories are significantly different or not
# ------------------------------------------------------------------------------

plotellipses(res.mca, keepvar = c("restaurant", "place.of.purchase", "relaxant", "profession"))




# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a single categorical variable
# ------------------------------------------------------------------------------

# keepvar = 11:  restaurant
plotellipses(res.mca, keepvar = 11, label = "none")

