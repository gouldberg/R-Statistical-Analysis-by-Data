setwd("//media//kswada//MyFiles//R//bike_sharing")

packages <- c("dplyr", "caret", "lattice", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  bike sharing  -->  create data by "01_bike_sharing_basics.R" scripts
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# set my theme for visualization
# ------------------------------------------------------------------------------
library("ggplot2")
library("viridis")

# define graphics theme
my_theme = function(legend.position='right'){
  theme_bw() %+replace%
    theme(legend.position=legend.position)
}

theme_set(my_theme())

default_color = "azure4"



# ------------------------------------------------------------------------------
# Lasso regression model
#   - Lasso is an automatic and convenient way to introduce sparsity into the linear regression model. 
#     Lasso stands for “least absolute shrinkage and selection operator” and, when applied in a linear regression model, 
#     performs feature selection and regularization of the selected feature weights
# ------------------------------------------------------------------------------
bike.features.of.interest <- c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]

y = bike[,'cnt']

dat = cbind(X, y)


# ----------
library("glmnet")

X.d = model.matrix(y ~ . -1, data = X)

l.mod = glmnet(X.d, y)

plot(l.mod,  xvar = "lambda", ylab="Weights")


# --> 
# What value should we choose for lambda ?
# If you see the penalization term as a tuning parameter, then you can find the lambda that minimizes the model error with cross-validation.
# You can also consider lambda as a parameter to control the interpretability of the model.
# The larger the penalization, the fewer features are present in the model (because their weights are zero) and the better the model can be interpreted.



# ------------------------------------------------------------------------------
# Lasso regression model
# ------------------------------------------------------------------------------
l.mod$beta


# Let us first set the number to 2 features:
extract.glmnet.effects = function(betas, best.index) {
  data.frame(beta = betas[, best.index])
}


# Let us first set the number to 2 features:
# The first two features with non-zero weights in the Lasso path are temperature (“temp”) and the time trend (“days_since_2011”).
n.features = apply(l.mod$beta, 2, function(x){sum(x!=0)})
kable(extract.glmnet.effects(l.mod$beta, max(which(n.features == 2))), col.names = "Weight", digits = 2)


# Now, let us select 5 features:
kable(extract.glmnet.effects(l.mod$beta, max(which(n.features == 5))), col.names = "Weight", digits = 2)



# -->
# Note that the weights for “temp” and “days_since_2011” differ from the model with two features. 
# The reason for this is that by decreasing lambda even features that are already “in” the model are penalized less and may get a larger absolute weight.
# The interpretation of the Lasso weights corresponds to the interpretation of the weights in the linear regression model.
# You only need to pay attention to whether the features are standardized or not, because this affects the weights.
# In this example, the features were standardized by the software, but the weights were automatically transformed back for us to match the original feature scales.



# Linear regression models can only represent linear relationships, i.e. a weighted sum of the input features.
# Each nonlinearity or interaction has to be hand-crafted and explicitly given to the model as an input feature.
# Linear models are also often not that good regarding predictive performance, because the relationships that can be learned are so restricted and usually oversimplify how complex reality is.
# The interpretation of a weight can be unintuitive because it depends on all other features.
# A feature with high positive correlation with the outcome y and another feature might get a negative weight in the linear model, because, given the other correlated feature, it is negatively correlated with y in the high-dimensional space. 


