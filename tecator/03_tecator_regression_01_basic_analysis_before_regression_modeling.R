setwd("//media//kswada//MyFiles//R//tecator")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tecotor
# ------------------------------------------------------------------------------
data("tecator", package = "caret")

dim(absorp)
dim(endpoints)

car::some(absorp)
car::some(endpoints)


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .3, .4, .5)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2

trellis.par.set(theme1)



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  correlation, nearZeroVar, and skewness
# ------------------------------------------------------------------------------
# Overall correlation
library(corrplot)

par(mrfow=c(1,1))
corrplot::corrplot(cor(absorp),  order = "hclust",  tl.cex = .8)



# ----------
# 99 out of 100 variables have pair-wise correlation above 0.90 in absolute mean !!!!
length(findCorrelation(cor(absorp), cutoff = 0.90))


# --> The predictors have a high degree of correlation.


# ----------
# near zero variance variable
nearZeroVar(absorp)



# ----------
# skewness
summary(apply(absorp, MARGIN=2, FUN=skewness))


# -->
# average skewness statistic is 0.9027 (with minimum of 0.8260 and a maximum of 0.9976),
# indicating that these predictors doet not have large skewness.

round(apply(absorp, MARGIN=2, FUN=skewness), digits=3)



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  principal component analysis
# ------------------------------------------------------------------------------
pca_obj <- prcomp(data.frame(absorp), center = TRUE, scale. = TRUE)

plot(pca_obj)

plot(round(cumsum(pca_obj$sdev^2 / sum(pca_obj$sdev^2)), digits = 3),
     type = "p", pch = "*", main = "cum % of variance accounted", ylab="")


# --> Only 1 or 2 principal components are effective...


