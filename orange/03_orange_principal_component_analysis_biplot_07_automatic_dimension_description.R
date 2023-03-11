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
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca <- FactoMineR::PCA(orange, quanti.sup = 8:14, quali.sup = 15:16)



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
# lapply(dimdesc(res.pca),lapply,round,2)



# -->
# The variables which best characterise component 1 are odour typicality, sweetness, bitterness,m and acidicity
# (Only component 1 is applicable by dimdesc() ...?)

# "Origin" as the correlation is significantly different from 0.
# The coordinates of the orange juices from Florida are significantly higher than average on the first component,
# whereas the coordinates of the other orange juices are lower than average.






