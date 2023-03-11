setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# impute missng values by mice
# ------------------------------------------------------------------------------

library(mice)


# impute separately treated and untreated groups
long.imputation <- c()


# quickpred: create matrix of zeros and ones where the rows indicate the variables to be imputed and the columns indicate the predictors selected for each variable.
# This allows the creation of an imputation model for each covariate, where the mincor = 0.1 argument specifies that a predictor is included only if
# its Pearson correlation with the covariate has an absolute value of at least 0.1, and the minpuc = 0.5 argument specifies that predictors should have
# at least 50 % of usable cases.
# With the exclude = c("STU_ID") argument, the student id variable is excluded.

# The mice function with the m = 5 argument obtains five multiple imputations by chained equations.
# The method = "pmm" argument specifies predictive mean matching (PMM) as the univariate imputation model,
# while the visitSequence = "monotone" argument specifies an imputing order from the variable with the least missing data to the variable with the most missing data.

# The imputed data sets are stored withing the long.imputation object.
# The complete function of the mice package allows extracting imputed data sets in a variety of formats: single imputed data sets, stacked imputed data sets in a long format,
# and a wide data set with imputed data sets side by side.
# Here the action = "long" argument is used to specify that the imputed data sets will be stacked in a long format.

# Although a different type of univariate imputation model could have been specified for each variable, in this example, the model specification
# is simplified by using PMM as the univariate imputation method for all variables.
# PMM can handle imputing both continuous and categorical variables. PMM is a 2-step process that first computes predicted values for the variable being imputed.
# Then, for each missing value, PMM randomly samples an observed value from a set of candidate donors that have predicted values close to the predicted values of the missing value.
# PMM has several advantages: (1) It does not require the specification of an explicit model for the distribution of missing values,
# (2) it guarantees that imputed values are always withing the range of observed values, and
# (3) it produces imputed missing values with similar distributions as the observed values, which is particularly useful if the observed variable
# is nonnormally distributed.


for (group in 0:1) {
  
  # creates a list of predictors of missing data with a mininum correlation of 0.1 and at least 50% of useful data
  predictor.selection <- quickpred(subset(ELS.data, treat == group), mincor = 0.1, minpuc = 0.5, method = 'pearson', exclude = c("STU_ID"))
  
  
  # impute variables by from least missing to most missing
  # Using multiple imputation by chained equations
  # with predictive mean matching as the univariate imputation method
  imputation <- mice(subset(ELS.data, treat == group), m = 5, method = "pmm", visitSequence = "monotone", predictorMatrix = predictor.selection)
  
  
  # extract stacked data files
  long.imputation = rbind(long.imputation, complete(imputation, action = "long"))
  
}




# ------------------------------------------------------------------------------
# create imputtionList
#   - mitools::imputationList() is specifically designed to integrate with the survey package for design-based estimation with complex survey data.
# ------------------------------------------------------------------------------

# extract a single imputation dataset
imputation1 <- subset(long.imputation, subset=.imp == 1)


head(imputation1)



# ----------
# create a list of all imputed datasets
# load package to combine imputed datasets

library(mitools)


# this object was specifically designed to be analyzed with the survey package
allImputations <- imputationList(list(
  subset(long.imputation, subset=.imp==1),
  subset(long.imputation, subset=.imp==2),
  subset(long.imputation, subset=.imp==3),
  subset(long.imputation, subset=.imp==4), 
  subset(long.imputation, subset=.imp==5)))


str(allImputations)



