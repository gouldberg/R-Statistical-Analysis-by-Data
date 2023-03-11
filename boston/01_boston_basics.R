setwd("//media//kswada//MyFiles//R//boston")

packages <- c("dplyr", "caret", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Boston
# ------------------------------------------------------------------------------
data("Boston", package = "MASS")

data <- Boston

dim(data)

str(data)

car::some(data)



# ------------------------------------------------------------------------------
# basics:  pairwise correlation
# ------------------------------------------------------------------------------
psych::pairs.panels(data)




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



# ------------------------------------------------------------------------------
# basics:  principal component analysis
# ------------------------------------------------------------------------------
pr_trans <- prcomp(~ ., data = data_trans, scale. = TRUE)

pr_trans

cumsum(round(pr_trans$sdev^2 / sum(pr_trans$sdev^2), digits = 3))


# --> 
# 4 PCs are required to account for more than 75% of variance


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



# ------------------------------------------------------------------------------
# basics:  try to remove variables with high correlation
# ------------------------------------------------------------------------------
highCorr <- findCorrelation(cor(data_trans), cutoff = 0.75)

length(highCorr)



# ----------
# half of columns are removed
data_filtered <- data_trans[, -highCorr]

dim(data_filtered)







