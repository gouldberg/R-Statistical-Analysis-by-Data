setwd("//media//kswada//MyFiles//R//sine_curve")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "kernlab")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: sine curve
# ------------------------------------------------------------------------------
set.seed(100)

x <- runif(100, min = 2, max = 10)

y <- sin(x) + rnorm(length(x)) * 0.25

( sinData <- data.frame(x = x, y = y) )


graphics.off();  par(mfrow=c(1,1))

plot(x, y)



# ------------------------------------------------------------------------------
# Investigate the relationship between the cost, epsilon for a support vector machine model
# ------------------------------------------------------------------------------
# Greate a grid of x values to use for prediction
( dataGrid <- data.frame(x = seq(2, 10, length = 100)) )



# ----------
# generate prediction by SVM with radial basis function and plot fitted curve for each parameter set
# cols = colorRampPalette(c("#0068b7","white","#f39800"))
cols = rep(c("black", "red", "blue", "orange", "darkgray", "navy", "yellow", "tan"), 20)


# Create parameter grid by Cost with epsilon fixed by 0.0001
( param_grid <- expand.grid(C = 2^(-4:-2), epsilon = 10^(-4:-4)) )


# Create parameter grid by epsilon with cost fixed by 0.5 or 0.25
( param_grid <- expand.grid(C = 2^(-1:-1), epsilon = 10^(0:-4)) )
( param_grid <- expand.grid(C = 2^(-2:-2), epsilon = 10^(-4:0)) )


# Create parameter grid by Cost with epsilon fixed by 0.1
( param_grid <- expand.grid(C = 2^(-4:2), epsilon = 10^(-1:-1)) )


graphics.off();  par(mfrow=c(1,1))
plot(x, y)

modelPrediction <- matrix(nrow=100, ncol=nrow(param_grid))

for(i in 1:nrow(param_grid)){
  print(paste0(i, "  processing:  C = ", param_grid[i,1], "    epsilon = ", param_grid[i,2]))
  rbfSVM <- ksvm(x = x, y = y, data = sinData, kernel = "rbfdot", kpar = "automatic", C = param_grid[i,1], epsilon = param_grid[i,2])
  modelPrediction[,i] <- predict(rbfSVM, newdata = dataGrid)
#  points(x = dataGrid$x, y = modelPrediction[,i], type = "l", col = cols(i))
  points(x = dataGrid$x, y = modelPrediction[,i], type = "l", col = cols[i])
}



# -->
# When cost value is increased, fitting will be better but become over-fitting

# When epsilon values is decreased, fitting will be better but become over-fitting
# but fitting does not change much with epsilon value under 0.1

# Cost paramter is more important



# ------------------------------------------------------------------------------
# Investigate the relationship between the cost, epsilon and kernel parameters (sigma) for a support vector machine model
# ------------------------------------------------------------------------------
# generate prediction by SVM with radial basis function and plot fitted curve for each parameter set
# cols = colorRampPalette(c("#0068b7","white","#f39800"))
cols = rep(c("black", "red", "blue", "orange", "darkgray", "navy", "yellow", "tan"), 20)


# Create parameter grid by kernel parameter (sigma) with cost and epsilon fixed
( param_grid <- expand.grid(C = 2^(2:2), epsilon = 10^(-1:-1), sigma = 2^(-4:4)) )


graphics.off();  par(mfrow=c(1,1))
plot(x, y)

modelPrediction <- matrix(nrow=100, ncol=nrow(param_grid))

for(i in 1:nrow(param_grid)){
  print(paste0(i, "  processing:  C = ", param_grid[i,1], "    epsilon = ", param_grid[i,2], "    sigma = ", param_grid[i,3]))
  rbfSVM <- ksvm(x = x, y = y, data = sinData, kernel = "rbfdot", kpar = list(sigma = param_grid[i,3]), C = param_grid[i,1], epsilon = param_grid[i,2])
  modelPrediction[,i] <- predict(rbfSVM, newdata = dataGrid)
 #  points(x = dataGrid$x, y = modelPrediction[,i], type = "l", col = cols(i))
 points(x = dataGrid$x, y = modelPrediction[,i], type = "l", col = cols[i])
                 
}



# -->
# When sigma value is increased, fitting will be better but become over-fitting



# ------------------------------------------------------------------------------
# Find best parameters by cross-validation
# ------------------------------------------------------------------------------
set.seed(100)

# create folds explicitly, default is 10 folds
indx <- createFolds(y, returnTrain = TRUE)
indx


ctrl <- trainControl(method = "cv", index = indx)



# Grid search of 14 cost values between 2^-2, 2^-1, 2, 2^2, ..., 2^11
# Radial basis function has a parameter (sigma) that controls the scale.
# In the radial basis function, sigma is estimated using combinations of the training set points to calculate the distribution of (x - x')^2,
# then use the 10th and 90th percentiles as a range for sigma.
svmRTune <- train(x = data.frame(x = sinData$x), y = sinData$y,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 14,
                  trControl = ctrl)

svmRTune



# ----------
# the model used 75 traiing set data points as support vectors out pf 100 data points.
# kernel parameter sigma is estimated analytically to be 7.051
# Also optimal C = 2^2, epsilon = 0.1
svmRTune$finalModel



# ----------
plot(svmRTune, scales = list(x = list(log = 2)))                 



# ------------------------------------------------------------------------------
# polynomial SVM model
#  - We tune over the cost, the polynomial degree, and a scale factor.
#  - In general, quadratic models have smaller error rates than the linear models. Also, models associated with larger-scale factors have better performance.
# ------------------------------------------------------------------------------
svmGrid <- expand.grid(degree = 1:2, scale = c(1, 0,1, 0.01, 0.005, 0.001), C = 2^(-2:5))

set.seed(100)

svmPTune <- train(x = data.frame(x = sinData$x), y = sinData$y,
                  method = "svmPoly",
                  preProc = c("center", "scale"),
                  tuneGrid = svmGrid,
                  trControl = ctrl)


svmPTune



# ----------
# The final model was fit using degree 1 model with a scale factor of 0.005 and Cost =  8 and epsilon = 0.1
svmPTune$finalModel
plot(svmPTune, scales = list(x = list(log = 2), between = list(x = .5, y = 1)))                 



# ------------------------------------------------------------------------------
# plot fitted curve by best model
# ------------------------------------------------------------------------------
graphics.off();  par(mfrow=c(1,1))
plot(x, y)
points(x = dataGrid$x, y = predict(svmRTune, dataGrid), type = "l", col = "red")
points(x = dataGrid$x, y = predict(svmPTune, dataGrid), type = "l", col = "blue")




# ------------------------------------------------------------------------------
# data: sine curve + outliers
#  - Check that support vector machine is robust for outliers
# ------------------------------------------------------------------------------
set.seed(100)

x2 <- c(runif(100, min = 2, max = 10), 2.1, 2.4, 2.7, 3.0, 3.3)
y2 <- sin(x2) + rnorm(length(x2)) * 0.25


# ----------
# set outliers
y2[101:105] <- c(-1.4, -1.3, -1.4, -1.3, -1.4)

( sinData2 <- data.frame(x = x2, y = y2) )

graphics.off();  par(mfrow=c(1,1))
plot(x2, y2)


set.seed(100)

indx2 <- createFolds(y2, returnTrain = TRUE)

ctrl2 <- trainControl(method = "cv", index = indx2)

svmRTune2 <- train(x = data.frame(x = sinData2$x), y = sinData2$y,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 14,
                  trControl = ctrl)

svmRTune2


( dataGrid2 <- data.frame(x = seq(2, 10, length = 105)) )

graphics.off();  par(mfrow=c(1,1))
plot(x2, y2)
points(x = dataGrid2$x, y = predict(svmRTune2, dataGrid2), type = "l", col = "red")



# ----------
# but support vector machines regression does not predict at the out of training range
( dataGrid3 <- data.frame(x = seq(2, 15, length = 200)) )
graphics.off();  par(mfrow=c(1,1))
plot(x = dataGrid3$x, y = predict(svmRTune2, newdata = dataGrid3), type = "l", col = "red")
