setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca <- FactoMineR::PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)




# ------------------------------------------------------------------------------
# Automatic dimension description
# ------------------------------------------------------------------------------

dimdesc(res.pca)


dimdesc(res.pca)$Dim.1

dimdesc(res.pca)$Dim.2


# -->
# 1st component is mainly due to the variable number of points
# (with a correlation coefficient of 0.96) and the variable 100m (with a negative correlation of -0.77)
# the 2nd component is described by two quantitative variables only (discus and shot put).
# No category of any categorical variable characterises components 1 and 2 with a confidence level of 95%



# ----------
# change threshold
dimdesc(res.pca, proba = 0.2)



# -->
# With this confidence level, we can say that both of the two categories Olympic Games and Decastar
# have coordinates that are significantly different from 0 on the first component
# As the value is positive (negative) for the Olympic Games (Decastar) we can say that individuals who participated in the Olympic Games
# tend to have positive coordinates (or negative, respectively) on component 1.


