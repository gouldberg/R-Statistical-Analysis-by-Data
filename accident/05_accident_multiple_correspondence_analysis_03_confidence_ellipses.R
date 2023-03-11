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
# Confidence ellipses around categories of a categorical variable
# (i.e., around the barycentre of the individuals carrying that category)
# to assess whether two categories are significantly different or not
# ------------------------------------------------------------------------------

plotellipses(res.mca, keepvar = c("mode", "age", "gender"))




# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a single categorical variable
# ------------------------------------------------------------------------------

# keepvar = 3:  mode
plotellipses(res.mca, keepvar = 3)

