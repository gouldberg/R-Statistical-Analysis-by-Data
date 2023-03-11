setwd("//media//kswada//MyFiles//R//blood_brain")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  BloodBrain
#   - QSAR(Quantitative Structure-Activity Relationship) data set from Mente and Lombardo (2005).
#   - QSAR is modeling where the characteristics of a chemical compound are used to predict other chemical properties.
#   - Here, the ability of a chemical to permeate the blood-brain barrier was experimentally determined for 208 compounds.
#   - 134 descriptors were measured for each compound
# ------------------------------------------------------------------------------
data("BloodBrain", package = "caret")

data <- bbbDescr
y <- logBBB

dim(data)
str(data)

y



# ------------------------------------------------------------------------------
# basics:  check skewness
# ------------------------------------------------------------------------------
PP <- preProcess(data, method = "BoxCox")

PP


# -->
# all 40 out of 40 variables coudld be Box-Cox transformed



# ------------------------------------------------------------------------------
# Apply data transformation
# ------------------------------------------------------------------------------
data_trans <- predict(PP, data)

car::some(data_trans)




# ------------------------------------------------------------------------------
# basics:  between-predictors correlations
# ------------------------------------------------------------------------------
library(corrplot)

corrplot(cor(data_trans), order = "hclust")


# -->
# Correlations of between-predictors are not strong.



# ------------------------------------------------------------------------------
# basics:  principal component analysis
# ------------------------------------------------------------------------------
pr_trans <- prcomp(~ ., data = data_trans, scale. = TRUE)

pr_trans

cumsum(round(pr_trans$sdev^2 / sum(pr_trans$sdev^2), digits = 3))


# --> 
# 9 PCs are required to account for more than 75% of variance


# ----------
transparentTheme(pchSize = .8, trans = .3)

panelRange <- extendrange(pr_trans$x[, 1:9])
panelRange


splom(as.data.frame(pr_trans$x[, 1:9]),
      groups = data$Type,
      type = c("p", "g"),
      as.table = TRUE,
      auto.key = list(columns = 2),
      prepanel.limits = function(x) panelRange)


# -->
# It seems that data reduction is very difficult


# ------------------------------------------------------------------------------
# basics:  try to remove variables with high correlation
# ------------------------------------------------------------------------------
highCorr <- findCorrelation(cor(data_trans), cutoff = 0.75)

length(highCorr)



# ----------
# half of columns are removed
data_filtered <- data_trans[, -highCorr]

dim(data_filtered)







