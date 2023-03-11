setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ----------
rownames(HarvardPsych)

colnames(HarvardPsych)




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca0 <- FactoMineR::PCA(HarvardPsych)



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

library(FactoMineR)


dimdesc(res.pca0)



# ----------
lapply(dimdesc(res.pca0),lapply,round,2)



# -->
# The variables which best characterise component 1 are american, society, association ...
# (Only component 1 is applicable by dimdesc() ...?)






