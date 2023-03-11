# setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_時系列\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------


data <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(data)


dim(data)


car::some(data)




# ------------------------------------------------------------------------------
# split data
# ------------------------------------------------------------------------------

data$date


# 200808 and before
train_idx <- 1:92


# 200809 and after
test_idx <- 93:132


train <- data[train_idx,]


test <- data[test_idx,]


data$class <- c(rep(0, length(train_idx)), rep(1, length(test_idx)))





# ------------------------------------------------------------------------------
# bivariate pairs plot
# ------------------------------------------------------------------------------


library(car)


formula <- ~ TXN + MU + INTC + TSM + PFE + MRK + LLY + JPM + MS + GS | class


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# -->
# MRK is almost same before and after
# investment bank is really changed



# ------------------------------------------------------------------------------
# Correlation Network by train and test data
# ------------------------------------------------------------------------------


# correlation matrix
cormat_train <- cor(train[,2:11])

cormat_test <- cor(test[,2:11])



# ----------
library(qgraph)


# minimum = 0.2:  lower absolute correlation threshold to 0.2 (i.e., small-medium effect size)

cornet_train <- qgraph(cormat_train, layout = "spring", minimum = 0.2, graph = "cor",
                 groups = list(semi = c(1,2,3,4), medi = c(5,6,7), invb = c(8,9,10)),
                 color = c("white", "gray", "orange"), labels = colnames(data[,2:11]))


cornet_test <- qgraph(cormat_test, layout = "spring", minimum = 0.2, graph = "cor",
                 groups = list(semi = c(1,2,3,4), medi = c(5,6,7), invb = c(8,9,10)),
                 color = c("white", "gray", "orange"), labels = colnames(data[,2:11]))




# ----------
# strength:  number of edgs connected with a node
# z-standardized centrality values

centralityPlot(cornet_train)

centralityPlot(cornet_test)






# ------------------------------------------------------------------------------
# Correlation Network:  by Graphical Lasso
# ------------------------------------------------------------------------------

graphics.off()

glanet_train <- qgraph(cormat_train, layout = "spring", sampleSize = nrow(train), graph = "glasso",
                 groups = list(semi = c(1,2,3,4), medi = c(5,6,7), invb = c(8,9,10)),
                 color = c("white", "gray", "orange"), labels = colnames(data[,2:11]))



glanet_test <- qgraph(cormat_test, layout = "spring", sampleSize = nrow(test), graph = "glasso",
                       groups = list(semi = c(1,2,3,4), medi = c(5,6,7), invb = c(8,9,10)),
                       color = c("white", "gray", "orange"), labels = colnames(data[,2:11]))



# ------------------------------------------------------------------------------
# Time-varying graphical lasso
# ------------------------------------------------------------------------------

graphics.off()


span <- 40



# ----------
# for example:  starting at 40th time point
i <- 40
# i <- 41

start_idx <- i - span + 1

end_idx <- start_idx - 1 + span

tmp <- data[start_idx : end_idx, 2:11]

cormat <- cor(tmp)

qgraph(cormat, layout = "spring", sampleSize = nrow(tmp), graph = "glasso",
                     groups = list(semi = c(1,2,3,4), medi = c(5,6,7), invb = c(8,9,10)),
                     color = c("white", "gray", "orange"), labels = colnames(data[,2:11]))



