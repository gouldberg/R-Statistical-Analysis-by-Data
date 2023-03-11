setwd("//media//kswada//MyFiles//R//friedman1")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "mlbench")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: friedman1
# ------------------------------------------------------------------------------
set.seed(200)

trainData <- mlbench.friedman1(200, sd = 1)

dim(trainData$x)

trainData$y


# ----------
# convert matrix to dafa frame, this will give the columns names
trainData$x <- data.frame(trainData$x)



# ----------
featurePlot(trainData$x, trainData$y)



# ----------
# This creats a list with a vector y and a matrix of predictors x, aso simulate a large test set to estimate the true error rate with good precision
testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)



# ----------
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .3, .4, .5)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)



# ------------------------------------------------------------------------------
# Target variable vs. continuous/categorical variable
# ------------------------------------------------------------------------------
featurePlot(trainData$x, trainData$y,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"), labels = rep("", 2))



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  correlation, nearZeroVar, and skewness
# ------------------------------------------------------------------------------
# Overall correlation
library(corrplot)

corrplot::corrplot(cor(trainData$x),  order = "hclust",  tl.cex = .8)



# ----------
# 0 out of 10 variables have pair-wise correlation above 0.75 in absolute mean.
length(findCorrelation(cor(trainData$x), cutoff = 0.75))



# ----------
# near zero variance variable
nearZeroVar(trainData$x)



# ----------
# skewness
summary(apply(trainData$x, MARGIN=2, FUN=skewness))



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  principal component analysis
# ------------------------------------------------------------------------------
pca_obj <- prcomp(trainData$x, center = TRUE, scale. = TRUE)

plot(pca_obj)

plot(round(cumsum(pca_obj$sdev^2 / sum(pca_obj$sdev^2)), digits = 3),
     type = "p", pch = "*", main = "cum % of variance accounted", ylab="")



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  linear dependencies
# ------------------------------------------------------------------------------
comboInfo <- findLinearCombos(trainData$x)

comboInfo



# ----------
# can be removed to eliminate the linear dependencies
comboInfo$remove



# ------------------------------------------------------------------------------
# BoxCox Transformation
# ------------------------------------------------------------------------------
trans <- preProcess(trainData$x, method = "BoxCox")

dat_trans <- predict(trans, trainData$x)

dat_trans



# ----------
summary(apply(dat_trans, MARGIN=2, FUN=skewness))

round(apply(dat_trans, MARGIN=2, FUN=skewness), digits=3)



# ------------------------------------------------------------------------------
# Target variable vs. continuous variable by featurePlot
# ------------------------------------------------------------------------------
# with not-transformed value
featurePlot(trainData$x, trainData$y,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))



# ----------
# with transformed value
featurePlot(trainData$x, trainData$y,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))

