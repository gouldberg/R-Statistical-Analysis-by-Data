setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



# ------------------------------------------------------------------------------
# split training and test data
#   - response variable:  siri  (the percentafe of body fat)
#   - predictors:  other variables except brozek and density
# ------------------------------------------------------------------------------
var <- c("siri", "age", "weight", "height", "adipos", "free", "neck", "chest", "abdom", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")


# take test data every tenth observation
test_id <- seq(10, nrow(fat), by = 10)

test_fat <- fat[test_id, var]



# ----------
# training data
train_id <- setdiff(1:nrow(fat), test_id)

train_fat <- fat[train_id, var]


nrow(test_fat)
nrow(train_fat)



# ------------------------------------------------------------------------------
# Fit linear regression with full variable
#   - response variable:  siri  (the percentafe of body fat)
#   - predictors:  other variables except brozek and density
# ------------------------------------------------------------------------------

lmod <- lm(siri ~ age + weight + height + adipos + free + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, data = train_fat)

summary(lmod)


rmse(train_fat$siri, lmod$fit)
rmse(test_fat$siri, predict(lmod, test_fat))



# ------------------------------------------------------------------------------
# model selection by AIC
# ------------------------------------------------------------------------------

lmod_step <- step(lmod, direction="both")

summary(lmod_step)


rmse(train_fat$siri, lmod_step$fit)
rmse(test_fat$siri, predict(lmod_step, test_fat))



