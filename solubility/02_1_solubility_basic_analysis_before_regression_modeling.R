setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  solubility
# ------------------------------------------------------------------------------
data("solubility", package = "AppliedPredictiveModeling")

ls(pattern = "^solT")


dim(solTrainX)

names(solTrainX)

head(solTestY)


str(solTrainX)


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .3, .4, .5)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2

trellis.par.set(theme1)



# ------------------------------------------------------------------------------
# Target variable vs. continuous/categorical variable
# ------------------------------------------------------------------------------
xyplot(solTrainY ~ solTrainX$MolWeight, type = c("p", "g"),
       ylab = "Solubility (log)",
       main = "(a)",
       xlab = "Molecular Weight")


xyplot(solTrainY ~ solTrainX$NumRotBonds, type = c("p", "g"),
       ylab = "Solubility (log)",
       xlab = "Number of Rotatable Bonds")


bwplot(solTrainY ~ ifelse(solTrainX[,100] == 1, "structure present", "structure absent"),
       ylab = "Solubility (log)", main = "(b)", horizontal = FALSE)



# ------------------------------------------------------------------------------
# Find the columns that are fingerprints (i.e. not the continuous predictors). 
# ------------------------------------------------------------------------------
fingerprints <- grep("FP", names(solTrainXtrans))
length(fingerprints)



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  correlation, nearZeroVar, and skewness
# ------------------------------------------------------------------------------
# Overall correlation
library(corrplot)

corrplot::corrplot(cor(solTrainXtrans[, -fingerprints]),  order = "hclust",  tl.cex = .8)



# ----------
# 10 out of 20 variables have pair-wise correlation above 0.75 in absolute mean.
length(findCorrelation(cor(solTrainX[, -fingerprints]), cutoff = 0.75))
length(findCorrelation(cor(solTrainX[, -fingerprints]), cutoff = 0.90))

length(findCorrelation(cor(solTrainX[, fingerprints]), cutoff = 0.90))



# ----------
# near zero variance variable
nearZeroVar(solTrainX[, -fingerprints])
nearZeroVar(solTrainX[, fingerprints])



# ----------
# skewness
summary(apply(solTrainX[, -fingerprints], MARGIN=2, FUN=skewness))

# -->
# average skewness statistic is 1.6 (with minimum of 0.7 and a maximum of 3.8),
# indicating that these predictors have a propensity to be right skewed.

round(apply(solTrainX[, -fingerprints], MARGIN=2, FUN=skewness), digits=3)




# ------------------------------------------------------------------------------
# Basic checks for descriptors:  principal component analysis
# ------------------------------------------------------------------------------
pca_obj <- prcomp(solTrainX, center = TRUE, scale. = TRUE)

plot(pca_obj)

plot(round(cumsum(pca_obj$sdev^2 / sum(pca_obj$sdev^2)), digits = 3),
     type = "p", pch = "*", main = "cum % of variance accounted", ylab="")



# ------------------------------------------------------------------------------
# Basic checks for descriptors:  linear dependencies
# ------------------------------------------------------------------------------
comboInfo <- findLinearCombos(solTrainX[, -fingerprints])

comboInfo



# ----------
# can be removed to eliminate the linear dependencies
comboInfo$remove



# ----------
tmp <- solTrainX[, -fingerprints]
tmp1 <- tmp[,comboInfo$linearCombos[[1]]]
tmp2 <- tmp[,comboInfo$linearCombos[[2]]]

splom(tmp1)

splom(tmp2)



# ------------------------------------------------------------------------------
# BoxCox Transformation
# ------------------------------------------------------------------------------
trans <- preProcess(solTrainX[, -fingerprints], method = "BoxCox")

dat_trans <- predict(trans, solTrainX[, -fingerprints])

dat_trans



# ----------
# average skewness is reduced from 1.6 to 1.3 (but still maximum of 3.8)
summary(apply(dat_trans, MARGIN=2, FUN=skewness))

round(apply(dat_trans, MARGIN=2, FUN=skewness), digits=3)



# ------------------------------------------------------------------------------
# Target variable vs. continuous variable by featurePlot
# ------------------------------------------------------------------------------
# with not-transformed value
featurePlot(solTrainX[, -fingerprints], solTrainY,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))



# ----------
# with transformed value
featurePlot(solTrainXtrans[, -fingerprints], solTrainY,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))


# -->
# The smoothed regression lines indicate that there are some linear relationships between the predictors and the outcome (e.g., molecular weight)
# and some nonlinear relationships (e.g., the number of origins or chlorines)
# Because of this, we might consider augmenting the predictor set wieht quadratic terms for some variables
