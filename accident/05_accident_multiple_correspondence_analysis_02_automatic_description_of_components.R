setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ----------
# convert table to case form
acci_c <- expand.dft(acci.tab)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(acci_c, quali.sup = 2)
# res.mca <- MCA(acci_c)



# ------------------------------------------------------------------------------
# Automatic description of the component
# ------------------------------------------------------------------------------

dimdesc(res.mca)$'Dim 1'$quali


# -->
# 1st component is characterised by the variables "mode" and "age" (in second)



# ----------
dimdesc(res.mca)$'Dim 1'$category


# -->
# Be careful for categorical variable with large number of unique values
# but this specifies the direction of the component.
# For example, the coordinate "gender=Pedestrian" is positive whereas the coordinate "mode-Motorcycle" is negative.



# ----------
# rounding
lapply(dimdesc(res.mca), lapply, round, 4)


lapply(dimdesc(res.mca), lapply, signif, 3)
