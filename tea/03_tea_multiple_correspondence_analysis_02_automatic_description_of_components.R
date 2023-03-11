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
# Automatic description of the component
# ------------------------------------------------------------------------------

dimdesc(res.mca)$'Dim 1'$quali


# -->
# 1st component is characterised by the variables "place of purchase", "tearoom", and so forth.
# Also, certain supplementary variables are also related to this component ("sex" and "friendliness")



# ----------
dimdesc(res.mca)$'Dim 1'$category


# -->
# Since most variables have two categories, characterisation by category is similar to that calculated from the variables,
# but specifies the direction of the component.
# For example, the coordinate "tearoom" is positive whereas the coordinate "not teamroom" is negative.
# Individuals with more positive coordinates would therefore be more likely to go to tearooms,



# ----------
# rounding
lapply(dimdesc(res.mca), lapply, round, 4)


lapply(dimdesc(res.mca), lapply, signif, 3)
