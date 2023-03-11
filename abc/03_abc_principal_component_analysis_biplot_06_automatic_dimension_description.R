setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ----------
# convert factor to numeric
ABC2 <- rapply(ABC[,c(1,4,6:11)], f = as.numeric, classes = "factor", how = "replace")

str(ABC2)



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca <- FactoMineR::PCA(ABC2, quanti.sup = 1:2)




# ------------------------------------------------------------------------------
# Automatic dimension description from the quantitative and categorical variables
#   - For a quantitative variable, the correlation coefficient between the coordinates of the individuals on the component and each variable
#     is calculated.
#     We then sort the variables in descending order from the highest coefficient to the weakest and retain the variables with the highest correlation coefficients
#   - For a categorical variable, we conduct a one-way analysis of variance where we seek to explain the coordinates of the individuals (on the component)
#     by the categorical variable.
#     We use the sum to zero contrasts (sum of all to 0)
#     For each categorical variable, a Student t-test is conducted to compare the average of the individuals who possess that category with the general average.
# ------------------------------------------------------------------------------

dimdesc(res.pca)



# ----------
lapply(dimdesc(res.pca),lapply,round,2)








