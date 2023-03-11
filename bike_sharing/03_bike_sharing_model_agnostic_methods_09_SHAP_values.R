setwd("//media//kswada//MyFiles//R//bike_sharing")

packages <- c("dplyr", "caret", "lattice", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  bike sharing  -->  create data by "01_bike_sharing_basics.R" scripts
# ------------------------------------------------------------------------------
bike.features.of.interest <- c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]

y = bike[,'cnt']

dat = cbind(X, y)



# ------------------------------------------------------------------------------
# SHAP Values
# ------------------------------------------------------------------------------
# Note: The functions shap.score.rank, shap_long_hd and plot.shap.summary were 
# originally published at https://liuyanguu.github.io/post/2018/10/14/shap-visualization-for-xgboost/
# All the credits to the author.




# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------
# return matrix of shap score and mean ranked score list
shap.score.rank <- function(xgb_model = xgb_mod, shap_approx = TRUE, 
                            X_train = mydata$train_mm){
  require(xgboost)
  require(data.table)
  shap_contrib <- predict(xgb_model, X_train,
                          predcontrib = TRUE, approxcontrib = shap_approx)
  shap_contrib <- as.data.table(shap_contrib)
  shap_contrib[,BIAS:=NULL]
  cat('make SHAP score by decreasing order\n\n')
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = (mean_shap_score)))
}



# ----------
# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}



# ----------
# prep shap data
shap.prep <- function(shap  = shap_result, X_train = mydata$train_mm, top_n){
  require(ggforce)
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
  require(data.table)
  shap_score_sub <- as.data.table(shap$shap_score)
  shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  
  # feature values: the values in the original dataset
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue; 
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2) 
}



# ----------
plot.shap.summary <- function(data_long){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 3, alpha = 0.7,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="#FFCC33", high="#6600CC", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") 
  return(plot1)
}



# ----------
var_importance <- function(shap_result, top_n=10)
{
  var_importance=tibble(var=names(shap_result$mean_shap_score), importance=shap_result$mean_shap_score)
  
  var_importance=var_importance[1:top_n,]
  
  ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
}



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(xgboost)
library(caret)


bike_2 = select(bike, -days_since_2011, -cnt, -yr)

bike_dmy = dummyVars(" ~ .", data = bike_2, fullRank=T)

bike_x = predict(bike_dmy, newdata = bike_2)

dim(bike_x)



# ----------
# Create the xgboost model
model_bike = xgboost(data = bike_x, nround = 10, objective="reg:linear", label= bike$cnt)  

model_bike



# ----------
# Calculate shap values
# `shap_approx` comes from `approxcontrib` from xgboost documentation. 
# Faster but less accurate if true. Read more: help(xgboost)
shap_result_bike = shap.score.rank(xgb_model = model_bike, X_train = bike_x, shap_approx = F)

shap_result_bike


# The shap value table has same dimension of dim(bike_x)
# The original values from the input data are replaced by its SHAP values. 
dim(shap_result_bike$shap_score)



# ----------
# Plot var importance based on SHAP
var_importance(shap_result_bike, top_n = 10)



# ----------
# Prepare data for top N variables
shap_long_bike = shap.prep(shap = shap_result_bike, X_train = bike_x, top_n = 10)

shap_long_bike

dim(shap_long_bike)



# ----------
# Plot shap overall metrics
plot.shap.summary(data_long = shap_long_bike)


# The y-axis indicates the variable name, in order of importance from top to bottom. The value next to them is the mean SHAP value.
# On the x-axis is the SHAP value. Indicates how much is the change in log-odds. From this number we can extract the probability of success.
# Gradient color indicates the original value for that variable. In booleans, it will take two colors, but in number it can contain the whole spectrum.
# Each point represents a row from the original dataset.


# -->
# We can see that having a high humidity is associated with high and negative values on the target. 
# Where high comes from the color and negative from the x value.
# In other words, people rent fewer bikes if humidity is high.
# When season.WINTER is high (or true) then shap value is high.
# People rent more bikes in winter, this is nice since it sounds counter-intuitive.
# Note the point dispersion in season.WINTER is less than in hum.



# ----------
# Visualizing the SHAP feature contribution to prediction dependencies on feature value.

xgb.plot.shap(data = bike_x, # input data
              model = model_bike, # xgboost model
              features = names(shap_result_bike$mean_shap_score[1:10]), # only top 10 var
              n_col = 3, # layout option
              plot_loess = T # add red line to plot
)


# Do some classical plots
# ggplotgui::ggplot_shiny(bike)


# Each blue dot is a row (a day in this case).
# Looking at temp variable, we can see how lower temperatures are associated with a big decrease in shap values.
# Interesting to note that around the value 22-23 the curve starts to decrease again. A perfect non-linear relationship.
# Taking mnth.SEP we can observe that dispersion around 0 is almost 0, while on the other hand, the value 1 is associated mainly with a shap increase around 200,
# but it also has certain days where it can push the shap value to more than 400.

# mnth.SEP is a good case of interaction with other variables, since in presence of the same value (1), the shap value can differ a lot.
