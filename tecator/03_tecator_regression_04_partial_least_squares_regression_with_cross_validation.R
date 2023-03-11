rm(list=ls())
setwd("//media//kswada//MyFiles//R//tecator")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tecator
# ------------------------------------------------------------------------------
data("tecator", package = "caret")

dim(absorp)
dim(endpoints)

car::some(absorp)
car::some(endpoints)

str(absorp)
str(endpoints)



# ------------------------------------------------------------------------------
# Preprocess data
# ------------------------------------------------------------------------------
trans <- preProcess(data.frame(absorp), method = c("BoxCox", "center", "scale", "pca"))
trans2 <- preProcess(data.frame(absorp), method = c("BoxCox", "center", "scale"))

trans
trans2

absorp_trans <- predict(trans, data.frame(absorp))
colnames(absorp_trans) <- c("PC1", "PC2")

absorp_trans2 <- predict(trans2, data.frame(absorp))

endpoints <- data.frame(endpoints)
colnames(endpoints) <- c("moisture", "fat", "protein")



# ------------------------------------------------------------------------------
# Split train and test data
# ------------------------------------------------------------------------------
absorp_trans$id <- 1:nrow(absorp_trans)
absorp_trans2$id <- 1:nrow(absorp_trans2)
endpoints$id <- 1:nrow(endpoints)


set.seed(100)

train_id <- sample(1:nrow(endpoints), size = floor(nrow(endpoints) * 0.75), replace = FALSE)
test_id <- setdiff(1:nrow(endpoints), train_id)

trainX <- absorp_trans %>% filter(id %in% train_id) %>% dplyr::select(-id)
testX <- absorp_trans %>% filter(id %in% test_id) %>% dplyr::select(-id)

trainX2 <- absorp_trans2 %>% filter(id %in% train_id) %>% dplyr::select(-id)
testX2 <- absorp_trans2 %>% filter(id %in% test_id) %>% dplyr::select(-id)



# ----------
# take "fat" for response
trainY <- endpoints %>% filter(id %in% train_id) %>% dplyr::select(fat)
testY <- endpoints %>% filter(id %in% test_id) %>% dplyr::select(fat)



# ----------
trainingData <- trainX
trainingData$fat <- trainY$fat

testData <- testX
testData$fat <- testY$fat

trainingData2 <- trainX2
trainingData2$fat <- trainY$fat

testData2 <- testX2
testData2$fat <- testY$fat



# ------------------------------------------------------------------------------
# Create folds explicitly and split train and test data
# ------------------------------------------------------------------------------
set.seed(100)

# create folds explicitly, default is 10 folds
indx <- createFolds(as.matrix(trainY), returnTrain = TRUE)

indx



# ------------------------------------------------------------------------------
# Set train control
# ------------------------------------------------------------------------------
ctrl <- trainControl(method = "cv", index = indx)



# ------------------------------------------------------------------------------
# Partial Least Squares regression model
#
#   - PCR (Principal Component Regression = dimension reduction + regression) does not necessarily produce new predictors that explain the response.
#     PCA does not consider any aspects of the response when it selects its components. Instead, it simply chases the variability present throughout
#   - PLS (Parical Least Squares) originated with Herman Wold's nonlinear iterative partial least squares (NIPALS) algorithm which linearized models
#     that were nonlinear in the parameters. Subsequently, Wold et al. (1983) adapted the NIPALS method for the regression setting with correlated predictors
#     and called this adaptation "PLS".
#   - While the PCA linear combinations are chosen to maximally summarize predictor space variability, the PLS linear combinations of predictors
#     are chosen to maximally summarize covariance with the response. This means that PLS finds components that maximally summarize the variation of the 
#     predictors while simultaneously requiring these components to have maximum correlation with the response.
#     PLS therefore strikes a compromise between the objectives of predictor space dimension reduction and a predictive relationship with the response.
#     In other words, PLS can be viewed as a supervised dimension reductin procedure; PCR is an unsupervised procedure.
# ------------------------------------------------------------------------------
# PLS has one tuning paramter, ncomp (number of components to be reatined)
# Cross-validation was used to determine the optimal number of PLS components to retain that minimize RMSE.
set.seed(100)

plsTune <- train(x = trainX2, y = trainY$fat,
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:20),
                 trControl = ctrl)

plsTune



# ----------
# PCR (Principal Component Regression)
set.seed(100)

pcrTune <- train(x = trainX2, y = trainY$fat,
                 method = "pcr",
                 tuneGrid = expand.grid(ncomp = 1:35),
                 trControl = ctrl)

pcrTune                  



# -->
# Note that the number of components required for PCR is greater than PLS.
# This is due to the fact that dimensions retained by PLS have been chosen to be optimally related to the response, while those chosen with PCR are not.



# ------------------------------------------------------------------------------
# Visualize number of components and RMSE (Cross-validation)
# ------------------------------------------------------------------------------
plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)

xyplot(RMSE ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",
       auto.key = list(columns = 2),
       groups = Model,
       type = c("o", "g"))



# ----------
# Visualize number of components and RMSE + 1 SD range
graphics.off()
par(mfrow=c(1,1))
tmp <- with(plsTune$results, data.frame(RMSE = RMSE, upper = RMSE + RMSESD, lower = RMSE - RMSESD))
matplot(tmp, type="b", pch=2:4)



# ------------------------------------------------------------------------------
# variable importance scores by PLS
# 
#  - Because the latent variables from PLS are constructed using linear combinations of the original predictors,
#    it is more difficult to quantify the relative contribution of each predictor to the model.
#    Wold et al. (1993) introduced a heuristic way to assess variable importance when using the NIPALS algorithm and
#    termed this calculation "variable importance in the projection".  (VIP)
# ------------------------------------------------------------------------------
plsImp <- varImp(plsTune, scale = FALSE)
plsImp

plot(plsImp, top = 25, scales = list(y = list(cex = .95)))




# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testResults_pls <- data.frame(obs = testY$fat, PLS = predict(plsTune, testX2))
testResults_pls <- testResults_pls %>% mutate(resid = obs - PLS)

testResults_pcr <- data.frame(obs = testY$fat, PCR = predict(pcrTune, testX2))
testResults_pcr <- testResults_pcr %>% mutate(resid = obs - PCR)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults_pls$obs, testResults_pls$PLS, testResults_pcr$PCR))

graphics.off()
par(mfrow = c(2,2))
with(testResults_pls, plot(obs, PLS, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_pls, plot(PLS, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testResults_pcr, plot(obs, PCR, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_pcr, plot(PCR, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# -->
# The residuals appear to be randomly scattered about 0 with respect to the predicted values.
# Both methods have similar predictive ability, but PLS does so with far fewer components



# ------------------------------------------------------------------------------
# Choosing Between Models
#   - Hothorn et. al. (2005) and Eugster et al. (2008) describe statistical methods for comparing methodologies
#     based on resampling results.
#     Since the accuracies were measured using identically resampled data sets, statistical methods for paired comparisons
#     can be used to determine if the differences between models are statistically significant.
#   - A paired t-test can be used to evaluate the hypothesis that the models have equivalent accuracirs (on average) or, 
#     analogously, that the mean difference in accuracy for the resampled data sets is zero.
# ------------------------------------------------------------------------------
# To compare two models based on their cross-validation statistics, the resamples function can be used with models
# that share a common set of resampled data sets.
# Since the random number seed was initialized prior to running the PLS and PCR models, paired accuracy measurements exist for each data set.

resamp <- resamples(list(PLS = plsTune, PCR = pcrTune))

summary(resamp)



# ----------
# resampled distribution of metric
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2

trellis.par.set(theme1)
bwplot(resamp, layout = c(1, 3), auto.key = list(columns = 2))

trellis.par.set(caretTheme())
densityplot(resamp, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 2), adjust = 1.5)



# ----------
modelDifferences <- diff(resamp)
summary(modelDifferences)



# ----------
# The actual paired t-test:
# confidence interval for the difference of RMSE is -0.084 - 0.186, indicating that there is evidence to support
# the idea that the RMSE for PCR is better than that for PLS
modelDifferences$statistics$RMSE

# -->
# p-values for the model comparisons are small for all MAE, RMSE, Rsquared
# which indicates that the models show successfully difference in performance.



# ----------
# resampled distribution of metric difference
trellis.par.set(theme1)
bwplot(modelDifferences, layout = c(1,3), auto.key = list(columns = 1))

trellis.par.set(caretTheme())
densityplot(modelDifferences, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 1))



# ------------------------------------------------------------------------------
# Extract model results from pls::plsr() and pls::pcr() function
#
#  - The pls packages has functions for PLS and PCR. SIMPLS, the first Dayal and MacGregor algorithm, and the algorithm developed by Rannar et al. (1994)
#    are each available.
#  - By default, the other algorithms can be specified using the method arguemnt using the values "oscorespls", "simpls", or "widekernelpls".
#  - The number of components can be fixed using the ncomp argument or, if left to the default, the maximum number of components
#    will be calculated.
# ------------------------------------------------------------------------------
# Parallelised cross-validation, using transient cluster
pls.options(parallel = 10)

plsFit <- pls::plsr(fat ~ ., data = trainingData2, scale = TRUE, center = TRUE, validation = c("CV"))
pcrFit <- pls::pcr(fat ~ ., data = trainingData2, scale = TRUE, center = TRUE, validation = c("CV"))



# ----------
# A contrast of the relationship between each of the first two PCR and PLS components with the fat response.
tmp1 <- data.frame(fat = trainY$fat, comp = plsFit$scores[,1], class = "PLScomp1")
tmp2 <- data.frame(fat = trainY$fat, comp = plsFit$scores[,2], class = "PLScomp2")
tmp3 <- data.frame(fat = trainY$fat, comp = pcrFit$scores[,1], class = "PCRcomp1")
tmp4 <- data.frame(fat = trainY$fat, comp = pcrFit$scores[,2], class = "PCRcomp2")
tmp <- rbind(tmp1, tmp2, tmp3, tmp4)

graphics.off()
trellis.par.set(theme1)
xyplot(fat ~ comp | class, data = tmp)


tmp %>% group_by(class) %>% summarize(COR=cor(fat, comp))




